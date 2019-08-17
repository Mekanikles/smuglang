#pragma once

#include <memory>
#include <initializer_list>

struct Type;
struct PrimitiveClass;
struct FunctionClass;
struct StructClass;
struct PointerClass;
struct TypeVariableClass;
struct ArrayClass;
struct TupleClass;
struct MultiTypeClass;

struct TypeRef;
bool isSubType(const Type& t, const TupleClass& tc);
bool isSubType(const Type& t, const MultiTypeClass& mtc);

using TypeId = unsigned int;

const int POINTER_SIZE_BYTES = 8;

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
		Struct,
		Pointer,
	};

	TypeClass(ClassType type) : type(type)
	{
		static TypeId s_typeId = 2; // Reseve 0 and 1 for Any and Void
		typeId = s_typeId++;
	}

	TypeClass(const TypeClass& o) = delete;
	TypeClass& operator=(const TypeClass& o) = delete;

	virtual std::unique_ptr<TypeClass> clone() const = 0;
	virtual string toString() const = 0;
	virtual bool isConcrete() const = 0;
	virtual bool ensureConcrete() = 0;
	virtual int getSize() const = 0;

	ClassType type = Any;
	TypeId typeId;
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
		Void,
		Value
	};

	Kind kind;
	std::unique_ptr<TypeClass> typeClass;

	Type() : kind(Kind::Any) {};
	Type(Kind kind)
		: kind(kind)
	{
		assert(kind != Kind::Value && "Must supply typeclass for value types");
	}
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

	int getSize() const
	{
		assert(isConcrete() && "Cannot take the size of inconcrete type");
		return typeClass->getSize();
	}

	TypeId typeId() const
	{
		if (kind == Any)
			return 0;
		if (kind == Void)
			return 1;
		assert(typeClass);
		return typeClass->typeId;
	}

	string toString() const
	{
		string s;
		if (kind == Any)
		{
			s = "Any";
			return s;
		}
		if (kind == Void)
		{
			s = "Void";
			return s;
		}

		assert(typeClass);
		s += typeClass->toString();
		s += string(" \033[1m#") + std::to_string(typeId()) + string("\033[22m");
		return s;
	}

	bool isConcrete() const
	{
		if (kind == Any)
			return false;
		else if (kind == Void)
			return true;
		else
			return typeClass->isConcrete();
	}

	bool ensureConcrete()
	{
		if (kind == Value)
			return typeClass->ensureConcrete();
		return false;	
	}

	bool operator==(const Type& o) const
	{
		if (this->kind == Any)
			return o.kind == Any;
		if (this->kind == Void)
			return o.kind == Void;	
		return this->kind == o.kind &&
			*this->typeClass == *o.typeClass;
	}

	bool isSubType(const Type& o) const
	{
		if (o.kind == Any)
			return true;

		// Some tuples can folded into the inner type
		if (!this->isTuple() && o.isTuple())
		{
			auto& tuple = o.getTuple();
			return ::isSubType(*this, tuple);
		}

		// Special 1:n case, where we need to compare a type with a typeclass
		if (!this->isMultiType() && o.isMultiType())
		{
			auto& multiType = o.getMultiType();
			return ::isSubType(*this, multiType);
		};

		return this->kind == o.kind &&
			this->typeClass->isSubClass(*o.typeClass);
	}

	bool isAny()
	{
		return kind == Any;
	}

	bool isVoid()
	{
		return kind == Void;
	}

	bool isFunction() const
	{
		return kind == Value && typeClass->type == TypeClass::Function;
	}

	bool isStruct() const
	{
		return kind == Value && typeClass->type == TypeClass::Struct;
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

	const StructClass& getStruct() const
	{
		assert(isStruct());
		return typeClass->as<StructClass>();
	}

	StructClass& getStruct()
	{
		assert(isStruct());
		return typeClass->as<StructClass>();
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

	const ArrayClass& getArray() const
	{
		assert(isArray());
		return typeClass->as<ArrayClass>();
	}

	ArrayClass& getArray()
	{
		assert(isArray());
		return typeClass->as<ArrayClass>();
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
		if (typeWrapper)
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

	int getSize() const
	{
		return typeWrapper->getType().getSize();
	}

	bool operator!=(const TypeRef& o) const
	{
		return !(getType() == o.getType());
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

	TypeRef& stripTrivialWrapperTypes();
};

TypeRef s_voidType(Type::Kind::Void);

////////////////////////////////////////////////////////////////////////////////

string typeString(const TypeRef& t)
{
	FGTextColor color;
	if (t->isConcrete())
	{
		if (t->isTuple())
			color = FGTextColor::BrightBlue;
		else
			color = FGTextColor::Magenta;
	}
	else
		color = FGTextColor::BrightRed;
		
	return prettyString(", Type: ", color, true) + prettyString(t.toString(), color);
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
		return knownSize && (signedType != UnknownSign || primitiveType == Float);
	}

	virtual bool ensureConcrete() override
	{
		switch (primitiveType)
		{
			case Int:
			{
				if (signedType == UnknownSign)
					signedType = Signed;
				if (!knownSize)
				{
					knownSize = true;
					size = 32;
				}
				break;
			}
			case Float:
			{
				if (!knownSize)
				{
					knownSize = true;
					size = 32;
				}
				break;
			}
			case Char:
			{
				if (signedType == UnknownSign)
					signedType = Unsigned;
				if (!knownSize)
				{
					knownSize = true;
					size = 8;
				}
				break;
			}
		}
		return true;
	}

	virtual int getSize() const override
	{
		assert(knownSize);
		return (int)size;
	}

	bool isInteger() const { return primitiveType == Int; }
	bool isFloat() const { return primitiveType == Float; }
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

	bool isStaticLength() const
	{
		return arrayType == Static;
	}

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
		{
			s += "Static Array";
			s += " (length: " + std::to_string(staticLength) + ")";
		}
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

	virtual bool ensureConcrete() override
	{
		return type->ensureConcrete();
	}

	virtual int getSize() const override
	{
		// TODO: Handle dynamic arrays
		//	we must know the implementation of dyn arrays here
		assert(arrayType == Static);
		// TODO: Handle align?
		return staticLength * type.getSize();
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
		if (unbounded)
			return false;

		for (const auto& t : types)
		{
			if (!t->isConcrete())
				return false;
		}
		return true;
	}

	virtual bool ensureConcrete() override
	{
		if (unbounded)
			return false;

		for (auto& t : types)
		{
			if (!t->ensureConcrete())
				return false;
		}

		return true;
	}	

	virtual int getSize() const override
	{
		// Hm, can tuples have a size?
		assert(false);
		return (int)0;
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
			string s;
			if (!identifier.empty())
			{
				s += identifier;
				s += " : ";
			}
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

	void convertToVariadicIfPossible()
	{
		if (!inParams.empty())
		{
			auto& p = inParams.back();
			if (p.type->isTuple() && p.type->getTuple().unbounded)
			{
				inParams.pop_back();
				isCVariadic = true;
			}
		}
	}	

	virtual bool ensureConcrete() override
	{
		for (auto& p : inParams)
		{
			if (!p.type->ensureConcrete())
				return false;
		}

		for (auto& p : outParams)
		{
			if (!p.type->ensureConcrete())
				return false;
		}

		return true;
	}

	virtual int getSize() const override
	{
		// Function types is always the size of a pointer
		return POINTER_SIZE_BYTES;
	}	

	bool operator==(const FunctionClass& o) const
	{
		if (isCVariadic != o.isCVariadic)
			return false;

		size_t inParamCount = inParams.size();
		if (inParamCount!= o.inParams.size())
			return false;

		size_t outParamCount = inParams.size();
		if (outParamCount != o.outParams.size())
			return false;

		for (int i = 0; i < inParamCount; ++i)
		{
			if (!(inParams[i].type == o.inParams[i].type))
				return false;
		}

		for (int i = 0; i < outParamCount; ++i)
		{
			if (!(outParams[i].type == o.outParams[i].type))
				return false;
		}

		return true;
	}

	bool isSubClass(const FunctionClass& o) const
	{
		return true;
	}
};

struct StructClass : TypeClass
{
	struct Field
	{
		string name;
		TypeRef type;

		Field clone() const
		{
			return Field{name, type.clone()};
		}

		string toString() const
		{
			string s;
			if (!name.empty())
			{
				s += name;
				s += " : ";
			}
			s += type.toString();
			return s;
		}
	};

	string name;
	vector<Field> fields;

	StructClass(string name)
		: TypeClass(TypeClass::Struct)
		, name(name)
	{}

	void addField(TypeRef&& _type, const string name)
	{
		fields.push_back({name, std::move(_type)});
	}

	const Field* getFieldByName(const string& name) const
	{
		for (auto& f : fields)
		{
			if (f.name == name)
				return &f;
		}
		return nullptr;
	}

	Field* getFieldByName(const string& name)
	{
		for (auto& f : fields)
		{
			if (f.name == name)
				return &f;
		}
		return nullptr;
	}

	int getFieldIndexByName(const string& name) const
	{
		int c = 0;
		for (auto& f : fields)
		{
			if (f.name == name)
				return c;
			c++;
		}
		return -1;
	}

	virtual std::unique_ptr<TypeClass> clone() const override
	{
		auto structObj = new StructClass(name);
		for (auto& f : fields)
		{
			structObj->fields.push_back(f.clone());
		}

		return std::unique_ptr<StructClass>(structObj);
	}

	virtual string toString() const override
	{
		string s = "Struct ";
		s += name;
		s += "(";
		for (int i = 0, e = fields.size(); i < e; ++i)
		{
			auto& p = fields[i];
			s += p.toString();
			if (i < e - 1)
				s += ", ";
		}	
		s += ")";
		return s;
	}

	virtual bool isConcrete() const override
	{
		return true;
	}

	virtual bool ensureConcrete() override
	{
		return true;
	}

	virtual int getSize() const override
	{
		return 0;
	}	

	bool operator==(const StructClass& o) const
	{
		return true;
	}

	bool isSubClass(const StructClass& o) const
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

	virtual bool ensureConcrete() override
	{
		return type->ensureConcrete();
	}

	virtual int getSize() const override
	{
		return POINTER_SIZE_BYTES;
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

	virtual bool ensureConcrete() override
	{
		return type->ensureConcrete();
	}	

	virtual int getSize() const override
	{
		// Type id is the size of a pointer atm. Should always be 64bit?
		return POINTER_SIZE_BYTES;
	}	

	bool operator==(const TypeVariableClass& o) const
	{
		return type == o.type;
	}

	bool isSubClass(const TypeVariableClass& o) const
	{
		return type.isSubType(o.type);
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
	
	virtual bool ensureConcrete() override
	{
		return false;
	}

	virtual int getSize() const override
	{
		assert(false);
		return -1;
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

bool isSubType(const Type& t, const TupleClass& tc)
{
	// Allow folding single-type tuples into the inner type
	// TODO: Should we just strip the tuple type on expression processing instead?
	if (tc.types.size() == 1)
	{
		if (t.isMultiType())
		{
			// TODO: Hm, flipped check, does that matter?
			return tc.types.back().getType().isSubType(t);
		}
		else
		{
			return t.isSubType(tc.types.back().getType());
		}
	}
	return false;
}

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
	
	if (o.type == TypeVariable)
		return this->compareAs<TypeVariableClass>(&o);	
	else if (o.type == Primitive)
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

	if (o.type == TypeVariable)
		return o.type == this->type && this->isSubClassAs<TypeVariableClass>(&o);	
	else if (o.type == Primitive)
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

Type createTypeVariable()
{
	return Type(std::make_unique<TypeVariableClass>(Type()));
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

Type createStructType(string name)
{
	return Type(std::make_unique<StructClass>(name));
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

struct UnificationResult
{
	enum ChangeStatusFlag
	{
		NoUnification = 0x00,
		LeftChanged = 0x01,
		RightChanged =  0x02,
	};

	struct PendingChange
	{
		TypeRef& source;
		TypeRef& target;
	};

	vector<PendingChange> pendingChanges;
	uint status;

	UnificationResult()
		: status(NoUnification)
	{}

	UnificationResult(ChangeStatusFlag statusFlag)
		: status(statusFlag)
	{}

	void addPendingChange(TypeRef& source, TypeRef& target)
	{
		pendingChanges.emplace_back(PendingChange{ source, target });
	}

	operator bool() const
	{
		return status != NoUnification;
	}

	void merge(UnificationResult&& other)
	{
		status |= other.status;
		pendingChanges.reserve(pendingChanges.size() + other.pendingChanges.size());
		for (auto& change : other.pendingChanges)
		{
			pendingChanges.emplace_back(std::move(change));
		}

		other.pendingChanges.clear();
	}

	void apply()
	{
		for (auto& change : pendingChanges)
		{
			change.source.mergeInto(change.target);
		}
		pendingChanges.clear();
	}
};

UnificationResult createLeftChange(TypeRef& left, TypeRef& right)
{
	UnificationResult result(UnificationResult::LeftChanged);
	result.addPendingChange(left, right);
	return result;
}

UnificationResult createRightChange(TypeRef& left, TypeRef& right)
{
	UnificationResult result(UnificationResult::RightChanged);
	result.addPendingChange(right, left);
	return result;
}

UnificationResult generateMultiTypeUnification(TypeRef& leftType, TypeRef& rightType);

UnificationResult generateTypeUnification(TypeRef& leftType, TypeRef& rightType)
{
	TypeRef& innerLeftType = leftType.stripTrivialWrapperTypes();
	TypeRef& innerRightType = rightType.stripTrivialWrapperTypes();

	if (innerLeftType == innerRightType || innerRightType.isSubType(innerLeftType))
	{
		return createLeftChange(innerLeftType, innerRightType);
	}
	else if (innerLeftType.isSubType(innerRightType))
	{
		return createRightChange(innerLeftType, innerRightType);
	}

	return generateMultiTypeUnification(innerLeftType, innerRightType);
}

bool unifyTypes(TypeRef& leftType, TypeRef& rightType)
{
	auto result = generateTypeUnification(leftType, rightType);
	result.apply();
	return (bool)result;
}

UnificationResult generateMultiTypeUnification(TypeRef& leftType, TypeRef& rightType)
{
	UnificationResult result;

	if (!leftType.getType().isMultiType() || !rightType.getType().isMultiType())
		return result;

	auto& leftMultiType = leftType.getType().getMultiType();
	auto& rightMultiType = leftType.getType().getMultiType();
	for (TypeRef& leftSubType : leftMultiType.types)
	{
		for (TypeRef& rightSubType : rightMultiType.types)
		{
			auto subResult = generateTypeUnification(leftSubType, rightSubType);
			if (subResult)
			{
				result.merge(std::move(subResult));
			}
		}
	}

	return result;
}

bool isCharPointer(const Type& type)
{
	const PointerClass* pc = type.isPointer() ? &type.getPointer() : nullptr;
	if (pc && pc->type->isPrimitive() && pc->type->getPrimitive().isChar())
	{
		return true;
	}
	return false;
}

bool isStringType(const Type& type)
{
	const ArrayClass* ac = type.isArray() ? &type.typeClass->as<ArrayClass>() : nullptr;
	if (ac && ac->type->isPrimitive() && ac->type->getPrimitive().isChar())
	{
		return true;
	}

	return isCharPointer(type);
}

TypeRef& TypeRef::stripTrivialWrapperTypes()
{
	if (getType().isTuple())
	{
		auto& tuple = getType().getTuple();
		if (tuple.types.size() == 1)
			return tuple.types[0].stripTrivialWrapperTypes();
	}
	else if(getType().isMultiType())
	{
		auto& mtype = getType().getMultiType();
		if (mtype.types.size() == 1)
			return mtype.types[0].stripTrivialWrapperTypes();
	}
	
	return *this;
}

