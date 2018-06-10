
#include <memory>
#include <initializer_list>

struct Type;
struct PrimitiveClass;
struct FunctionClass;
struct PointerClass;
struct ArrayClass;
struct TupleClass;
struct MultiTypeClass;

bool isSubType(const Type& t, const MultiTypeClass& mtc);

struct TypeClass
{
	enum ClassType
	{
		Any, // TODO: Necessary? Type can be Any also
		MultiType,
		Primitive,
		Array,
		Tuple,
		Function,
		Pointer,
	};

	TypeClass(ClassType type) : type(type)
	{}

	TypeClass(const TypeClass& o) = delete;
	TypeClass& operator=(const TypeClass& o) = delete;

	virtual std::unique_ptr<TypeClass> clone() const = 0;

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

	template<typename T>
	T& as()
	{
		return *static_cast<T*>(this);
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
	Type(Kind kind, std::shared_ptr<TypeClass> typeClass)
		: kind(kind), typeClass(typeClass)
	{}
	Type(bool isTypeVariable, std::shared_ptr<TypeClass> typeClass)
		: typeClass(typeClass)
	{
		assert(typeClass.get());
		kind = isTypeVariable ? TypeVariable : Value;
	}

	Type clone() const
	{
		return Type(kind, typeClass ? typeClass->clone() : nullptr);
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

		// Special 1:n case, where we need to compare a type with a typeclass
		if (!this->isMultiType() && o.isMultiType())
		{
			auto& t1 = *this;
			auto& multiType = o.getMultiType();
			return ::isSubType(t1, multiType);
		};

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

	bool isTuple() const
	{
		return kind == Value && typeClass->type == TypeClass::Tuple;
	}

	const FunctionClass& getFunction() const
	{
		assert(isFunction());
		return typeClass->as<FunctionClass>();
	}

	FunctionClass& getFunction()
	{
		assert(isFunction());
		return typeClass->as<FunctionClass>();
	}

	const TupleClass& getTuple() const
	{
		assert(isTuple());
		return typeClass->as<TupleClass>();
	}

	TupleClass& getTuple()
	{
		assert(isTuple());
		return typeClass->as<TupleClass>();
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

	bool isMultiType() const 
	{
		return kind == Value && typeClass->type == TypeClass::MultiType;
	}

	MultiTypeClass& getMultiType()
	{
		assert(isMultiType());
		return typeClass->as<MultiTypeClass>();
	}

	const MultiTypeClass& getMultiType() const
	{
		assert(isMultiType());
		return typeClass->as<MultiTypeClass>();
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

	virtual std::unique_ptr<TypeClass> clone() const override
	{
		return std::make_unique<PrimitiveClass>(primitiveType, knownSize, size, signedType);
	}

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

	virtual std::unique_ptr<TypeClass> clone() const override
	{
		return std::make_unique<ArrayClass>(arrayType, type.clone(), staticLength);
	}

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

struct TupleClass : TypeClass
{
	vector<Type> types;
	// Note, if unbounded is true, we only use first type for all elements
	bool unbounded;

	TupleClass(vector<Type> types) 
		: TypeClass(TypeClass::Tuple)
		, types(types)
		, unbounded(false)
	{}

	TupleClass()
		: TypeClass(TypeClass::Tuple)
		, unbounded(true)
	{}

	virtual std::unique_ptr<TypeClass> clone() const override
	{
		auto tuple = new TupleClass();
		for (auto& t : types)
		{
			tuple->types.push_back(t.clone());
		}
		tuple->unbounded = unbounded;

		return std::unique_ptr<TupleClass>(tuple);
	}

	bool operator==(const TupleClass& o) const
	{
		if (types.size() != o.types.size())
			return false;

		for (int i = 0, s = types.size(); i < s; ++i)
		{
			auto&& t1 = types[i];
			auto&& t2 = o.types[i];

			if (!(t1 == t2))
				return false;
		}

		return true;
	}

	bool isSubClass(const TupleClass& o) const
	{
		if (types.size() != o.types.size())
			return false;

		for (int i = 0, s = types.size(); i < s; ++i)
		{
			auto&& t1 = types[i];
			auto&& t2 = o.types[i];

			if (!t1.isSubType(t2))
				return false;
		}

		return true;
	}
};

struct FunctionClass : TypeClass
{
	vector<Type> inTypes;
	vector<Type> outTypes;
	bool isCVariadic = false;

	// TODO: Figure out subtyping for functions
	FunctionClass() 
		: TypeClass(TypeClass::Function)
	{}	

	virtual std::unique_ptr<TypeClass> clone() const override
	{
		auto function = new FunctionClass();
		for (auto& t : inTypes)
		{
			function->inTypes.push_back(t.clone());
		}
		for (auto& t : outTypes)
		{
			function->outTypes.push_back(t.clone());
		}	
		function->isCVariadic = isCVariadic;

		return std::unique_ptr<FunctionClass>(function);
	}

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

	virtual std::unique_ptr<TypeClass> clone() const override
	{
		return std::make_unique<PointerClass>(type.clone());
	}

	bool operator==(const PointerClass& o) const
	{
		return true;
	}

	bool isSubClass(const PointerClass& o) const
	{
		return true;
	}
};

struct MultiTypeClass : TypeClass
{
	vector<Type> types;

	MultiTypeClass(std::initializer_list<Type> types) 
		: TypeClass(TypeClass::MultiType)
		, types(types)
	{}

	MultiTypeClass(vector<Type> types) 
		: TypeClass(TypeClass::MultiType)
		, types(types)
	{}	

	virtual std::unique_ptr<TypeClass> clone() const override
	{
		vector<Type> typesClone;
		for (auto& t : types)
		{
			typesClone.push_back(t.clone());
		}

		return std::make_unique<MultiTypeClass>(std::move(typesClone));
	}

	void addType(const Type& type)
	{
		types.push_back(type);
	}

	bool operator==(const MultiTypeClass& o) const
	{
		return true;
	}

	bool isSubClass(const MultiTypeClass& o) const
	{
		for (const Type& t1 : types)
		{
			bool foundSubType = false;
			for (const Type& t2 : o.types)
			{
				if (t1.isSubType(t2))
				{
					foundSubType = true;
					break;
				}
			}

			if (!foundSubType)
				return false;
		}

		return true;
	}
};

bool isSubType(const Type& t, const MultiTypeClass& mtc)
{
	for (const Type& t2 : mtc.types)
	{
		if (t.isSubType(t2))
			return true;
	}

	return false;
}

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
	else if (o.type == MultiType)
		return this->compareAs<MultiTypeClass>(&o);
	else if (o.type == Tuple)
		return this->compareAs<TupleClass>(&o);

	// Any
	return true;
}

bool TypeClass::isSubClass(const TypeClass& o) const
{
	if (o.type == TypeClass::Any)
		return true;

	if (o.type == Primitive)
		return o.type == this->type && this->isSubClassAs<PrimitiveClass>(&o);
	else if (o.type == Array)
		return o.type == this->type && this->isSubClassAs<ArrayClass>(&o);
	else if (o.type == Function)
		return o.type == this->type && this->isSubClassAs<FunctionClass>(&o);
	else if (o.type == Pointer)
		return o.type == this->type && this->isSubClassAs<PointerClass>(&o);
	else if (o.type == MultiType)
		return o.type == this->type && this->isSubClassAs<MultiTypeClass>(&o);
	else if (o.type == Tuple)
		return o.type == this->type && this->isSubClassAs<TupleClass>(&o);

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

Type createTupleType(vector<Type> types)
{
	return Type(false, std::make_shared<TupleClass>(std::move(types)));
}

Type createFunctionType()
{
	return Type(false, std::make_shared<FunctionClass>());
}

Type createTypeVariable(std::unique_ptr<TypeClass> typeClass)
{
	return Type(true, std::move(typeClass));
}

template<class ...Ts>
Type createMultiTypeVariable(Ts... types)
{
	return Type(false, std::make_shared<MultiTypeClass>(std::initializer_list<Type>{types...}));
}

Type createPointerType(const Type& type)
{
	return Type(false, std::make_shared<PointerClass>(type));
}

enum UnificationResult
{
	NoChange,
	LeftChanged,
	RightChanged,
	BothChanged,
	CannotUnify,
};

bool tryUnifyMultiTypes(Type& leftType, Type& rightType);

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

	if (tryUnifyMultiTypes(leftType, rightType))
		return BothChanged;

	return CannotUnify;
}

bool tryUnifyMultiTypes(Type& leftType, Type& rightType)
{
	if (!leftType.isMultiType() || !rightType.isMultiType())
		return false;

	vector<Type> typeIntersection;

	auto& leftMultiType = leftType.getMultiType();
	auto& rightMultiType = leftType.getMultiType();
	for (const Type& t1 : leftMultiType.types)
	{
		for (const Type& t2 : rightMultiType.types)
		{
			Type tc1 = t1;
			Type tc2 = t2;
			const auto result = unifyTypes(tc1, tc2);
			if (result != CannotUnify)
			{
				Type tr;
				if (result == LeftChanged)
					tr = tc1;
				else if (result == RightChanged)
					tr = tc2;
				else if (result == BothChanged)
					tr = tc1;

				typeIntersection.push_back(tr);
				break;
			}
		}
	}

	if (!typeIntersection.empty())
	{
		leftMultiType.types = typeIntersection;
		rightType = leftType;
		return true;
	}

	return false;
}









