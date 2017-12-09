
#include <memory>

/*enum class PrimitiveType
{
	None,
	s32,
};

string toString(PrimitiveType type)
{
	switch (type)
	{
		case PrimitiveType::s32: return "signed 32bit int";
		default: return "Uknown Primitive";
	};
}

struct Type
{
	bool isFunction = false;
	bool isInt = false;
	bool isFloat = false;
	bool isString = false;
	bool isType = false;
	PrimitiveType type;

	bool operator ==(const Type &b) const
	{
		return isFunction == b.isFunction &&
			isInt == b.isInt &&
			isFloat == b.isFloat &&
			isString == b.isString && 
			isType == b.isType && 
			type == b.type;
	}

	string toString()
	{
		return string("Type(Func: ") + std::to_string(isFunction) +
			", Int: " + std::to_string(isInt) +
			", Float: " + std::to_string(isFloat) +
			", Str: " + std::to_string(isString) + 
			", Type: " + std::to_string(isType) + ")";
	}
};

bool isAssignable(const Type& type1, const Type& type2)
{
	if (type2.isInt)
	{
		if (type1.type == PrimitiveType::s32)
			return true;
	}

	return false;
}
*/

struct TypeClass
{
	enum ClassType
	{
		Any,
		Primitive,
		Array,
		Function
	};

	TypeClass(ClassType type) : type(type)
	{}

	ClassType type = Any;
	// TODO: symbol might not be known at time of unification
	//	and can be the same "name" but refer to different symbols
	//	because of variable/symbol shadowing. There can be a situation
	//	where it is not possible to know if a shadowing symbol will appear
	//	due to mixins/code generation later
	//struct Symbol* symbol;

	template<typename T>
	bool compareAs(const TypeClass* b) const
	{
		return *static_cast<const T*>(this) == *static_cast<const T*>(b);
	}

	template<typename T>
	bool isSubClassAs(const TypeClass* b) const
	{
		return static_cast<const T*>(this)->isSubClass(*static_cast<const T*>(b));
	}

	bool operator==(const TypeClass& o) const;
	bool isSubClass(const TypeClass& o) const;
};

struct Type
{
	enum Kind
	{
		Any,
		TypeVariable,
		Value
	};

	Kind kind;
	std::shared_ptr<TypeClass> typeClass;

	Type() : kind(Kind::Any) {};
	Type(bool isTypeVariable, std::shared_ptr<TypeClass> typeClass)
		: typeClass(typeClass)
	{
		kind = isTypeVariable ? TypeVariable : Value;
	}

	bool operator==(const Type& o) const
	{
		if (this->kind == Any)
			return o.kind == Any;
		return this->kind == o.kind &&
			*this->typeClass == *o.typeClass;
	}

	bool isSubType(const Type& o) const
	{
		if (o.kind == Any)
			return true;
		return this->kind == o.kind &&
			this->typeClass->isSubClass(*o.typeClass);
	}

	bool isFunction()
	{
		return kind == Value && typeClass->type == TypeClass::Function;
	}
};

struct PrimitiveClass : TypeClass
{
	enum PrimitiveType
	{
		Int,
		Float,
		Char,
	};

	PrimitiveType primitiveType;
	bool knownSize = false;
	uint size = 32;

	PrimitiveClass(PrimitiveType primitiveType, bool knownSize, uint size) 
		: TypeClass(TypeClass::Primitive)
		, primitiveType(primitiveType)
		, knownSize(knownSize)
		, size(size)
	{}

	PrimitiveClass(PrimitiveType primitiveType, uint size)
		: PrimitiveClass(primitiveType, true, size)
	{}

	PrimitiveClass(PrimitiveType primitiveType)
		: PrimitiveClass(primitiveType, true, 0)
	{}

	bool operator==(const PrimitiveClass& o) const
	{
		return this->primitiveType == o.primitiveType &&
			this->knownSize == o.knownSize &&
			this->size == o.size;
	}

	bool isSubClass(const PrimitiveClass& o) const
	{
		if (this->primitiveType == o.primitiveType)
		{
			if (this->knownSize)
			{
				if (o.knownSize)
					return this->size == o.size;
				else
					return true;
			}
			else 
			{
				if (!o.knownSize)
					return true;
				else
					return false;
			}
		}

		return false;
	}	
};

struct ArrayClass : TypeClass
{
	enum ArrayType
	{
		AnyArray,
		Static,
		Dynamic,
	};

	ArrayType arrayType;
	Type type;
	uint staticLength;

	ArrayClass(ArrayType arrayType, const Type& type, uint staticLength = 0) 
		: TypeClass(TypeClass::Array)
		, arrayType(arrayType), type(type), staticLength(staticLength)
	{}

	bool operator==(const ArrayClass& o) const
	{
		return this->type == o.type &&
			this->arrayType == o.arrayType &&
			this->staticLength == o.staticLength;
	}

	bool isSubClass(const ArrayClass& o) const
	{
		if (this->type.isSubType(o.type))
		{
			if (o.arrayType == AnyArray)
				return true;
			else if (o.arrayType == Dynamic)
				return this->arrayType == Dynamic;

			return this->staticLength == o.staticLength;
		}

		return false;
	}
};

struct FunctionClass : TypeClass
{
	// TODO: Add signatures and figure out subtyping for functions
	FunctionClass() 
		: TypeClass(TypeClass::Function)
	{}	

	bool operator==(const ArrayClass& o) const
	{
		return true;
	}

	bool isSubClass(const ArrayClass& o) const
	{
		return true;
	}
};


bool TypeClass::operator==(const TypeClass& o) const
{
	if (this->type != o.type)
		return false;
	
	if (o.type == Primitive)
		return this->compareAs<PrimitiveClass>(&o);
	else if (o.type == Array)
		return this->compareAs<ArrayClass>(&o);

	// Any
	return true;
}

bool TypeClass::isSubClass(const TypeClass& o) const
{
	if (o.type == TypeClass::Any)
		return true;

	if (o.type != this->type)
		return false;

	if (o.type == Primitive)
		return this->isSubClassAs<PrimitiveClass>(&o);
	else if (o.type == Array)
		return this->isSubClassAs<ArrayClass>(&o);

	return false;
}

Type createPrimitiveType(PrimitiveClass::PrimitiveType primitiveType)
{
	return Type(false, std::make_shared<PrimitiveClass>(primitiveType));
}

Type createStaticArrayType(const Type& type, int length)
{
	return Type(false, std::make_shared<ArrayClass>(ArrayClass::Static, type, length));
}

Type createFunctionType()
{
	return Type(false, std::make_shared<FunctionClass>());
}

Type createTypeVariable(std::unique_ptr<TypeClass> typeClass)
{
	typeClass.release();
	return Type(true, std::move(typeClass));
}

enum UnificationResult
{
	NoChange,
	LeftChanged,
	RightChanged,
	CannotUnify,
};

UnificationResult unifyTypes(Type& leftType, Type& rightType)
{
	if (leftType == rightType)
	{
		return NoChange;
	}
	else if (rightType.isSubType(leftType))
	{
		leftType = rightType;
		return LeftChanged;
	}
	else if (leftType.isSubType(rightType))
	{
		rightType = leftType;
		return RightChanged;
	}

	return CannotUnify;
}









