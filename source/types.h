
#include <memory>
#include <initializer_list>

struct Type;
struct PrimitiveClass;
struct FunctionClass;
struct PointerClass;
struct TypeVariableClass;
struct ArrayClass;
struct TupleClass;
struct MultiTypeClass;

struct TypeRef;
bool isSubType(const Type& t, const MultiTypeClass& mtc);

struct TypeClass
{
	enum ClassType
	{
		Any,
		TypeVariable,
		MultiType,
		Primitive,
		Array,
		Tuple,
		Function,
		Pointer,
	};

	TypeClass(ClassType type) : type(type)
	{
		static unsigned int s_typeId = 0;
		typeId = s_typeId++;
	}

	TypeClass(const TypeClass& o) = delete;
	TypeClass& operator=(const TypeClass& o) = delete;

	virtual std::unique_ptr<TypeClass> clone() const = 0;
	virtual string toString() const = 0;
	virtual bool isConcrete() const = 0;

	ClassType type = Any;
	unsigned int typeId;
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
		Value
	};

	Kind kind;
	std::unique_ptr<TypeClass> typeClass;

	Type() : kind(Kind::Any) {};
	Type(Kind kind, std::unique_ptr<TypeClass> typeClass)
		: kind(kind), typeClass(std::move(typeClass))
	{}
	Type(std::unique_ptr<TypeClass> _typeClass)
		: typeClass(std::move(_typeClass))
	{
		assert(typeClass.get());
		kind = Value;
	}

	Type(const Type&) = delete;
	Type& operator=(const Type&) = delete;
	Type(Type&& o) = default;

	Type clone() const
	{
		return Type(kind, typeClass ? typeClass->clone() : nullptr);
	}

	string toString() const
	{
		string s;
		if (kind == Any)
		{
			s = "Any";
			return s;
		}

		assert(typeClass);
		s += typeClass->toString();
		s += string(" \033[1m#") + std::to_string(typeClass->typeId) + string("\033[22m");
		return s;
	}

	bool isConcrete() const
	{
		if (kind == Any)
			return false;
		else
			return typeClass->isConcrete();
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
		return hasTypeClass() && typeClass->type == TypeClass::TypeVariable;
	}

	const TypeVariableClass& getTypeVariable() const
	{
		assert(isTypeVariable());
		return typeClass->as<TypeVariableClass>();
	}

	TypeVariableClass& getTypeVariable()
	{
		assert(isTypeVariable());
		return typeClass->as<TypeVariableClass>();
	}

	bool hasTypeClass() const
	{
		return typeClass.get() != nullptr;
	}

	bool isAutoType() const
	{
		return !hasTypeClass();
	}
};

////////////////////////////////////////////////////////////////////////////////

// TODO: This is to be able to re-assign type references during
//	inference. It feels rather cumbersome, look into Algorithm W etc
struct TypeWrapper
{
	Type type;

	mutable vector<struct TypeRef*> references;

	TypeWrapper(Type&& type)
		: type(std::move(type))
	{
	}

	void addRef(TypeRef* typeRef) const
	{
		references.push_back(typeRef);
	}

	void removeRef(TypeRef* typeRef) const
	{
		auto it = std::find(references.begin(), references.end(), typeRef);
		if (it != references.end())
			references.erase(it);
	}

	Type& getType() { return type; }
	const Type& getType() const { return type; }
};

struct TypeRef
{
	std::shared_ptr<TypeWrapper> typeWrapper;

	TypeRef() 
	{
		typeWrapper = std::make_shared<TypeWrapper>(Type());
		typeWrapper->addRef(this);
	}

	TypeRef(Type&& type)
	{
		typeWrapper = std::make_shared<TypeWrapper>(std::move(type));
		typeWrapper->addRef(this);
	}

	TypeRef(const std::shared_ptr<TypeWrapper>& typeWrapper)
		: typeWrapper(typeWrapper)
	{
		typeWrapper->addRef(this);
	}

	TypeRef(const TypeRef& o)
		: typeWrapper(o.typeWrapper)
	{
		typeWrapper->addRef(this);
	}

	TypeRef(TypeRef&& o) 
	{
		o.typeWrapper->removeRef(&o);
		typeWrapper = std::move(o.typeWrapper);
		typeWrapper->addRef(this);
	}

	~TypeRef()
	{
		if (typeWrapper)
			typeWrapper->removeRef(this);
	}


	TypeRef& operator=(const TypeRef& o)
	{
		typeWrapper->removeRef(this);
		typeWrapper = o.typeWrapper;
		typeWrapper->addRef(this);
		return *this;
	}

	Type& getType() { return typeWrapper->getType(); }
	const Type& getType() const { return typeWrapper->getType(); }

	operator Type&() { return getType(); }
	operator const Type&() const { return getType(); }

	Type* operator->() { return &getType(); }
	const Type* operator->() const { return &getType(); }

	TypeRef clone() const
	{
		return TypeRef(typeWrapper->getType().clone());
	}

	string toString() const
	{
		return typeWrapper->getType().toString();
	}

	bool operator==(const TypeRef& o) const
	{
		return getType() == o.getType();
	}

	bool isSubType(const TypeRef& o) const
	{
		return getType().isSubType(o.getType());
	}

	void mergeInto(const TypeRef& o)
	{
		// Copy since merging will modify the original reference list
		auto refs = typeWrapper->references;

		for (TypeRef* ref : refs)
		{
			*ref = o;
		}
	}
};

////////////////////////////////////////////////////////////////////////////////

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

	virtual string toString() const override
	{
		string s;
		if (signedType == Signed)
			s += "Signed ";
		else if (signedType == Unsigned)
			s += "Unsigned ";

		if (primitiveType == Int)
			s += "Int";
		else if (primitiveType == Float)
			s += "Float";
		else if (primitiveType == Char)
			s += "Char";

		if (knownSize)
		{
			s += "(";
			s += std::to_string(size);
			s += ")";
		}

		return s;
	}

	virtual bool isConcrete() const override
	{
		return knownSize && signedType != UnknownSign;
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
	TypeRef type;
	uint staticLength;

	ArrayClass(ArrayType arrayType, TypeRef&& type, uint staticLength = 0) 
		: TypeClass(TypeClass::Array)
		, arrayType(arrayType), type(std::move(type)), staticLength(staticLength)
	{}

	virtual std::unique_ptr<TypeClass> clone() const override
	{
		return std::make_unique<ArrayClass>(arrayType, type.clone(), staticLength);
	}

	virtual string toString() const override
	{
		string s;
		if (arrayType == AnyArray)
			s += "Array";
		else if (arrayType == Static)
			s += "Static Array";
		else if (arrayType == Dynamic)
			s += "Dynamic Array";
		s += " of ";
		s += type.toString();
		return s;
	}

	virtual bool isConcrete() const override
	{
		return arrayType != AnyArray &&
			type->isConcrete();
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
	vector<TypeRef> types;

	// Note, if unbounded is true, we only use first type for all elements
	bool unbounded = false;

	TupleClass(vector<TypeRef>&& types) 
		: TypeClass(TypeClass::Tuple)
		, types(std::move(types))
		, unbounded(false)
	{}

	TupleClass(TypeRef&& type)
		: TypeClass(TypeClass::Tuple)
		, unbounded(true)
	{
		types.push_back(std::move(type));
	}

private:
	TupleClass()
		: TypeClass(TypeClass::Tuple)
	{
	}
public:

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

	virtual string toString() const override
	{
		string s;
		if (unbounded)
		{
			s += "Unbounded Tuple (";
			s += types.front().toString();
			s += ")";
			return s;
		}

		s += "Tuple (";
		for (int i = 0, e = types.size(); i < e; ++i)
		{
			auto& t = types[i];
			s += t.toString();
			if (i < e - 1)
				s += ", ";
		}
		s += ")";
		return s;
	}

	virtual bool isConcrete() const override
	{
		// TODO: Is tuple ever concrete? Unbounded tuples also?
		for (const auto& t : types)
		{
			if (!t->isConcrete())
				return false;
		}
		return true;
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
		if (o.unbounded)
		{
			// empty tuple always fit into unbounded tuples
			if (types.size() == 0)
				return true;

			// Check all types against unbounded type
			assert(o.types.size() == 1);
			auto&& t2 = o.types[0];
			for (int i = 0, s = types.size(); i < s; ++i)
			{
				auto&& t1 = types[i];
				if (!t1.isSubType(t2))
					return false;
			}
		}
		else
		{
			if (unbounded)
				return false;
			if (types.size() != o.types.size())
				return false;

			for (int i = 0, s = types.size(); i < s; ++i)
			{
				auto&& t1 = types[i];
				auto&& t2 = o.types[i];

				if (!t1.isSubType(t2))
					return false;
			}
		}

		return true;
	}
};

struct FunctionClass : TypeClass
{
	struct Param
	{
		string identifier;
		TypeRef type;

		Param clone() const
		{
			return Param{identifier, type.clone()};
		}

		string toString() const
		{
			if (identifier.empty())
				return "";
			string s = identifier;
			s += " : ";
			s += type.toString();
			return s;
		}
	};

	vector<Param> inParams;
	vector<Param> outParams;
	bool isCVariadic = false;

	// TODO: Figure out subtyping for functions
	FunctionClass() 
		: TypeClass(TypeClass::Function)
	{}	

	void appendInParam(TypeRef&& _type, const string id)
	{
		inParams.push_back({id, std::move(_type)});
	}

	void appendOutParam(TypeRef&& _type, const string id)
	{
		outParams.push_back({id, std::move(_type)});
	}

	virtual std::unique_ptr<TypeClass> clone() const override
	{
		auto function = new FunctionClass();
		for (auto& p : inParams)
		{
			function->inParams.push_back(p.clone());
		}
		for (auto& p : outParams)
		{
			function->outParams.push_back(p.clone());
		}	
		function->isCVariadic = isCVariadic;

		return std::unique_ptr<FunctionClass>(function);
	}

	virtual string toString() const override
	{
		string s = "Function (";
		for (int i = 0, e = inParams.size(); i < e; ++i)
		{
			auto& p = inParams[i];
			s += p.toString();
			if (i < e - 1)
				s += ", ";
		}
		if (isCVariadic)
			s += ", ...";
		s += ") -> (";
		for (int i = 0, e = outParams.size(); i < e; ++i)
		{
			auto& p = outParams[i];
			s += p.toString();
			if (i < e - 1)
				s += ", ";
		}
		s += ")";
		return s;
	}

	virtual bool isConcrete() const override
	{
		for (const auto& p : inParams)
		{
			if (!p.type->isConcrete())
				return false;
		}

		for (const auto& p : outParams)
		{
			if (!p.type->isConcrete())
				return false;
		}

		return true;
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
	TypeRef type;

	PointerClass(TypeRef&& type) 
		: TypeClass(TypeClass::Pointer)
		, type(std::move(type))
	{}	

	virtual std::unique_ptr<TypeClass> clone() const override
	{
		return std::make_unique<PointerClass>(type.clone());
	}

	virtual string toString() const override
	{
		string s = "Pointer(";
		s += type.toString();
		s += ")";
		return s;
	}

	virtual bool isConcrete() const override
	{
		return type->isConcrete();
	}

	bool operator==(const PointerClass& o) const
	{
		// TODO
		return true;
	}

	bool isSubClass(const PointerClass& o) const
	{
		// TODO
		return true;
	}
};

struct TypeVariableClass : TypeClass
{
	TypeRef type;

	TypeVariableClass(TypeRef&& type) 
		: TypeClass(TypeClass::TypeVariable)
		, type(std::move(type))
	{}	

	virtual std::unique_ptr<TypeClass> clone() const override
	{
		return std::make_unique<TypeVariableClass>(type.clone());
	}

	virtual string toString() const override
	{
		string s = "TypeVariable(";
		s += type.toString();
		s += ")";
		return s;
	}

	virtual bool isConcrete() const override
	{
		return type->isConcrete();
	}

	bool operator==(const TypeVariableClass& o) const
	{
		return true;
	}

	bool isSubClass(const TypeVariableClass& o) const
	{
		return true;
	}
};

struct MultiTypeClass : TypeClass
{
	vector<TypeRef> types;

	MultiTypeClass()
		: TypeClass(TypeClass::MultiType)
	{}

	MultiTypeClass(vector<TypeRef>&& types) 
		: TypeClass(TypeClass::MultiType)
		, types(std::move(types))
	{}	

	virtual std::unique_ptr<TypeClass> clone() const override
	{
		vector<TypeRef> typesClone;
		for (auto& t : types)
		{
			typesClone.push_back(t.clone());
		}

		return std::make_unique<MultiTypeClass>(std::move(typesClone));
	}
 
	virtual string toString() const override
	{
		string s = "MultiType (";
		for (int i = 0, e = types.size(); i < e; ++i)
		{
			auto& t = types[i];
			s += t.toString();
			if (i < e - 1)
				s += ", ";
		}
		s += ")";
		return s;
	}

	virtual bool isConcrete() const override
	{
		return false;
	}

	void appendType(TypeRef&& type)
	{
		types.push_back(std::move(type));
	}

	bool operator==(const MultiTypeClass& o) const
	{
		return true;
	}

	bool isSubClass(const MultiTypeClass& o) const
	{
		for (const TypeRef& t1 : types)
		{
			bool foundSubType = false;
			for (const TypeRef& t2 : o.types)
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
	for (const TypeRef& t2 : mtc.types)
	{
		if (t.isSubType(t2.getType()))
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

////////////////////////////////////////////////////////////////////////////////

Type createPrimitiveType(PrimitiveClass::PrimitiveType primitiveType)
{
	return Type(std::make_unique<PrimitiveClass>(primitiveType));
}

Type createStaticArrayType(TypeRef&& type, int length)
{
	return Type(std::make_unique<ArrayClass>(ArrayClass::Static, std::move(type), length));
}

Type createTypeVariable(TypeRef&& type)
{
	return Type(std::make_unique<TypeVariableClass>(std::move(type)));
}

Type createPointerTypeVariable(TypeRef&& type)
{
	assert(!type.getType().isTypeVariable());
	return createTypeVariable(Type(std::make_unique<PointerClass>(std::move(type))));
}

Type createTupleType(TypeRef&& type)
{
	return Type(std::make_unique<TupleClass>(std::move(type)));
}

Type createTupleType(vector<TypeRef>&& types)
{
	return Type(std::make_unique<TupleClass>(std::move(types)));
}

Type createFunctionType()
{
	return Type(std::make_unique<FunctionClass>());
}

Type createMultiTypeVariable()
{
	return Type(std::make_unique<MultiTypeClass>());
}

Type createPointerType(TypeRef&& type)
{
	return Type(std::make_unique<PointerClass>(std::move(type)));
}

////////////////////////////////////////////////////////////////////////////////

enum UnificationResult
{
	LeftChanged,
	RightChanged,
	BothChanged,
	CannotUnify,
};

bool tryUnifyMultiTypes(TypeRef& leftType, TypeRef& rightType);

UnificationResult unifyTypes(TypeRef& leftType, TypeRef& rightType)
{
	if (leftType == rightType || rightType.isSubType(leftType))
	{
		leftType.mergeInto(rightType);
		return LeftChanged;
	}
	else if (leftType.isSubType(rightType))
	{
		rightType.mergeInto(leftType);
		return RightChanged;
	}

	if (tryUnifyMultiTypes(leftType, rightType))
		return BothChanged;

	return CannotUnify;
}

bool tryUnifyMultiTypes(TypeRef& leftType, TypeRef& rightType)
{
	if (!leftType.getType().isMultiType() || !rightType.getType().isMultiType())
		return false;

	vector<TypeRef> typeIntersection;

	auto& leftMultiType = leftType.getType().getMultiType();
	auto& rightMultiType = leftType.getType().getMultiType();
	for (const TypeRef& t1 : leftMultiType.types)
	{
		for (const TypeRef& t2 : rightMultiType.types)
		{
			// TODO: Potential performance problem, n2 clone
			TypeRef tc1 = t1.clone();
			TypeRef tc2 = t2.clone();
			const auto result = unifyTypes(tc1, tc2);
			if (result != CannotUnify)
			{
				if (result == LeftChanged || result == BothChanged)
				{
					typeIntersection.push_back(std::move(tc1));
				}
				else if (result == RightChanged)
				{
					typeIntersection.push_back(std::move(tc2));
				}
				break;
			}
		}
	}

	if (!typeIntersection.empty())
	{
		leftMultiType.types = std::move(typeIntersection);
		rightType.mergeInto(leftType);
		return true;
	}

	return false;
}







