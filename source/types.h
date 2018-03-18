
#include <memory>

struct PrimitiveClass;
struct FunctionClass;
struct PointerClass;
struct ArrayClass;

struct TypeClass
{
	enum ClassType
	{
		Any,
		Primitive,
		Array,
		Function,
		Pointer
	};

	TypeClass(ClassType type) : type(type)
	{}

	TypeClass(const TypeClass& o) = delete;
	TypeClass& operator=(const TypeClass& o) = delete;

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

	template<typename T>
	const T& as() const
	{
		return *static_cast<const T*>(this);
	}
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
		assert(typeClass.get());
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

	bool isAny()
	{
		return kind == Any;
	}

	bool isFunction() const
	{
		return kind == Value && typeClass->type == TypeClass::Function;
	}

	const FunctionClass& getFunction() const
	{
		assert(isFunction());
		return typeClass->as<FunctionClass>();
	}

	bool isPrimitive() const 
	{
		return kind == Value && typeClass->type == TypeClass::Primitive;
	}

	const PrimitiveClass& getPrimitive() const
	{
		assert(isPrimitive());
		return typeClass->as<PrimitiveClass>();
	}

	bool isArray() const 
	{
		return kind == Value && typeClass->type == TypeClass::Array;
	}

	bool isPointer() const 
	{
		return kind == Value && typeClass->type == TypeClass::Pointer;
	}

	const PointerClass& getPointer() const
	{
		assert(isPointer());
		return typeClass->as<PointerClass>();
	}

	bool isTypeVariable() const
	{
		return kind == TypeVariable;
	}

	Type innerTypeFromTypeVariable() const
	{
		assert(isTypeVariable());
		return Type(false, typeClass);
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

	enum SignedType
	{
		UnknownSign,
		Signed, 
		Unsigned
	};

	PrimitiveType primitiveType;
	bool knownSize;
	uint size;
	SignedType signedType;

	PrimitiveClass(PrimitiveType primitiveType, bool knownSize, uint size, SignedType signedType) 
		: TypeClass(TypeClass::Primitive)
		, primitiveType(primitiveType)
		, knownSize(knownSize)
		, size(size)
		, signedType(signedType)
	{
		assert(signedType != Unsigned || primitiveType != Float);
	}

	PrimitiveClass(PrimitiveType primitiveType, uint size, SignedType signedType = UnknownSign)
		: PrimitiveClass(primitiveType, true, size, signedType)
	{}

	PrimitiveClass(PrimitiveType primitiveType, SignedType signedType = UnknownSign)
		: PrimitiveClass(primitiveType, false, 0, signedType)
	{}

	bool isInteger() const { return primitiveType == Int; }
	bool isChar() const { return primitiveType == Char; }
	bool isSigned() const { return signedType == Signed; }
	bool knowsSign() const { return signedType != UnknownSign; }
	bool knowsSize() const { return knownSize; }

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
	vector<Type> inTypes;
	vector<Type> outTypes;
	bool isVariadic = false;

	// TODO: Figure out subtyping for functions
	FunctionClass() 
		: TypeClass(TypeClass::Function)
	{}	

	bool operator==(const FunctionClass& o) const
	{
		return true;
	}

	bool isSubClass(const FunctionClass& o) const
	{
		return true;
	}
};

struct PointerClass : TypeClass
{
	Type type;

	PointerClass(const Type& type) 
		: TypeClass(TypeClass::Pointer)
		, type(type)
	{}	

	bool operator==(const PointerClass& o) const
	{
		return true;
	}

	bool isSubClass(const PointerClass& o) const
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
	else if (o.type == Function)
		return this->compareAs<FunctionClass>(&o);
	else if (o.type == Pointer)
		return this->compareAs<PointerClass>(&o);

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
	else if (o.type == Function)
		return this->isSubClassAs<FunctionClass>(&o);
	else if (o.type == Pointer)
		return this->isSubClassAs<PointerClass>(&o);

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

Type createPointerTypeVariable(const Type& type)
{
	assert(!type.isTypeVariable());
	return Type(true, std::make_shared<PointerClass>(type));
}

Type createFunctionType()
{
	return Type(false, std::make_shared<FunctionClass>());
}

Type createTypeVariable(std::unique_ptr<TypeClass> typeClass)
{
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









