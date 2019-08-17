#pragma once

#include <unordered_map>
#include "core.h"
#include "types.h"
#include "backend/value.h"
#include "symbols.h"

// TODO: Rename to "MIR"? Mid-level intermediate representation
namespace IR
{
	struct Scope;
	int getCurrentScopeOrder(Scope* scope);

	struct Defer;

	struct Value
	{
		enum class ValueType
		{
			Referenceable,
			Expression,
		};

		ValueType valueType;

		// TODO: Necessary? Can we generate backend without having to cache values?
		// Also, expressions probably does not need backendValue, maybe only on referenceables?
		//	Is Value used somewhere if we move down backendvalue then?
		Backend::Value* backendValue = nullptr;

		Value(ValueType valueType)
			: valueType(valueType)
		{}
		virtual const TypeRef& getType() const = 0;
	};

	struct Expression : Value
	{
		enum ExpressionType
		{
			Literal,
			Reference,
			ArrayAccess,
			MemberAccess,
			BinaryOp,
			UnaryOp,
			Call,
		};

		ExpressionType exprType;

		Expression(ExpressionType exprType) 
			: Value(Value::ValueType::Expression)
			, exprType(exprType) 
		{}
	
		virtual ~Expression() = default;
		virtual string toString() const = 0;
		virtual const vector<Expression*> getSubExpressions() { return {}; }
	};

	struct BinaryOp : Expression
	{
		enum OpType
		{
			Add,
			Sub,
			Mul,
			Div,
			Mod,
			Eq,
			LT,
			GT,
			LTE,
			GTE
		};

		const TypeRef type;

		OpType opType;
		unique<Expression> leftExpr;
		unique<Expression> rightExpr;

		BinaryOp(const TypeRef type, OpType opType, unique<Expression> leftExpr, unique<Expression> rightExpr) 
			: Expression(Expression::BinaryOp)
			, type(type)
			, opType(opType)
			, leftExpr(std::move(leftExpr))
			, rightExpr(std::move(rightExpr))
		{
		}

		virtual string toString() const override
		{
			switch (opType)
			{
				case Add: return "Add";
				case Sub: return "Sub";
				case Mul: return "Mul";
				case Div: return "Div";
				case Mod: return "Mod";
				case Eq: return "Eq";
				case LT: return "LT";
				case GT: return "GT";	
				case LTE: return "LTE";	
				case GTE: return "GTE";		
			}
			return "Unknown op";
		}

		virtual const vector<Expression*> getSubExpressions() override
		{ 
			return { &*leftExpr, &*rightExpr }; 
		}

		virtual const TypeRef& getType() const override { return this->type; }
	};

	struct UnaryOp : Expression
	{
		enum OpType
		{
			Neg,
		};

		const TypeRef type;

		OpType opType;
		unique<Expression> expr;

		UnaryOp(const TypeRef type, OpType opType, unique<Expression> expr) 
			: Expression(Expression::UnaryOp)
			, type(type)
			, opType(opType)
			, expr(std::move(expr))
		{
		}

		virtual string toString() const override
		{
			switch (opType)
			{
				case Neg: return "Neg";
			}
		}

		virtual const vector<Expression*> getSubExpressions() override
		{ 
			return { &*expr }; 
		}

		virtual const TypeRef& getType() const override { return this->type; }		
	};

	struct Literal final : Expression
	{
		const TypeRef type;

		// Hack: c-strings are not literals, since they are a pointer to
		//	data that cannot be a constant. Use this to indicate that we 
		//	need to fetch the correct address at compile time
		// TODO: Can we replace these literals with references instead?
		//	also, this is derived from the type, so don't store it like this
		bool isPointerType;

		vector<u8> data;

		Literal(const TypeRef type, vector<u8> data)
			: Expression(Expression::Literal)
			, type(type)
			, isPointerType(type->isPointer())
			, data(std::move(data))
		{
			this->backendValue = backendValue;
		}

		Literal(const TypeRef type)
			: Literal(type, vector<u8>())
		{
			this->backendValue = backendValue;
		}		

		template<typename T>
		T& readValue() const
		{
			assert(data.size() == sizeof(T));
			return *(T*)data.data();
		}

		unique<Literal> copy() const
		{
			return std::make_unique<Literal>(type, data);
		}

		virtual string toString() const override
		{
			string s = "Literal";
			return s;
		}

		virtual const TypeRef& getType() const override { return this->type; }
	};

	const struct Constant& asConstant(const struct Referenceable& ref);

	struct Referenceable : Value
	{
		enum class RefType
		{
			Variable,
			External,
			Constant
		};

		RefType refType;
		string name;

		// TODO: Need this for mapping to sources, for now
		const SymbolSource* symbolSource;

		Referenceable(RefType refType, string name, const SymbolSource* symbolSource)
			: Value(Value::ValueType::Referenceable)
			, refType(refType)
			, name(name)
			, symbolSource(symbolSource)
		{
		}

		const string getName() const { return name; }
		virtual const TypeRef& getType() const = 0;

		const bool isConstant() const
		{
			return refType == RefType::Constant;
		}

		const bool isExternal() const
		{
			return refType == RefType::External;
		}

		const struct Constant& asConstant() const
		{
			assert(refType == RefType::Constant);
			return ::IR::asConstant(*this);
		}
	};

	struct Reference : Expression
	{
		const Referenceable* referenceable;

		Reference(const Referenceable* referenceable) 
			: Expression(Expression::Reference) 
			, referenceable(referenceable)
		{}

		const Referenceable* getReferenceable() { return referenceable; }

		virtual string toString() const override
		{
			return prettyString(referenceable->getName(), FGTextColor::Blue, true);
		}

		virtual const TypeRef& getType() const override { return referenceable->getType(); }			
	};

	struct ArrayAccess : Expression
	{
		const TypeRef type;
		unique<Expression> baseExpr;
		unique<Expression> indexExpr;

		ArrayAccess(const TypeRef& type, unique<Expression> baseExpr, unique<Expression> indexExpr)
			: Expression(Expression::ArrayAccess)
			, type(type)
			, baseExpr(std::move(baseExpr))
			, indexExpr(std::move(indexExpr))
		{}

		virtual string toString() const override
		{
			string s = "ArrayAccess";
			return s;
		}

		virtual const TypeRef& getType() const override 
		{
			return type;
		}		
	};

	struct MemberAccess : Expression
	{
		unique<Expression> expression;
		string name;

		MemberAccess(unique<Expression> expression, string name) 
			: Expression(Expression::MemberAccess) 
			, expression(std::move(expression))
			, name(name)
		{}

		virtual string toString() const override
		{
			string s = expression->toString();
			s += ".";
			s += name;
			return s;
		}

		virtual const TypeRef& getType() const override 
		{ 
			const TypeRef& exprType = expression->getType(); 
			assert(exprType->isStruct());
			auto* field = exprType->getStruct().getFieldByName(name);
			assert(field);
			return field->type;
		}
	};

	struct Variable : Referenceable
	{
		const TypeRef type;	

		Variable(const TypeRef& type, string name, const SymbolSource* symbolSource) 
			: Referenceable(Referenceable::RefType::Variable, name, symbolSource)
			, type(type)
		{}

		Variable(Variable&& var)
			: Referenceable(Referenceable::RefType::Variable, std::move(var.name), var.symbolSource)
			, type(std::move(var.type))
		{
		}

		virtual const TypeRef& getType() const override { return type; }
	};

	struct StaticLinkable
	{
		const TypeRef type;
		string name;
		Backend::Value* backendValue = nullptr;

		StaticLinkable(const TypeRef type, string name)
			: type(type)
			, name(std::move(name))
		{
		}
	};

	struct External : Referenceable
	{
		std::shared_ptr<StaticLinkable> linkable;

		External(std::shared_ptr<StaticLinkable> linkable, const SymbolSource* symbolSource)
			: Referenceable(Referenceable::RefType::External, linkable->name, symbolSource)
			, linkable(linkable)
		{}

		virtual const TypeRef& getType() const override { return linkable->type; }	
	};

	struct Constant : Referenceable
	{
		shared<Literal> literal;

		Constant(shared<Literal> literal, string name, const SymbolSource* symbolSource) 
			: Referenceable(Referenceable::RefType::Constant, std::move(name), symbolSource) 
			, literal(literal)
		{}

		Constant(Constant&& con)
			: Referenceable(Referenceable::RefType::Constant, std::move(con.name), con.symbolSource)
			, literal(con.literal)
		{}

		virtual const TypeRef& getType() const override { return literal->getType(); }
	};

	const struct Constant& asConstant(const Referenceable& ref)
	{
		return static_cast<const struct Constant&>(ref);
	} 

	struct Call;
	struct Assignment;
	struct Conditional;

	struct Statement
	{
		enum StatementType
		{
			Scope,
			Assignment,
			Call,
			Conditional,
			Return,
			Loop,
			Continue,
			Break,
			Defer
		};

		int scopeLocalIndex = -1;
		StatementType statementType;

		Statement(StatementType statementType) 
			: statementType(statementType) 
		{}

		// WTF: Without virtual destructor, unique_ptr destructor crashes on double-free
		virtual ~Statement() = default;	
	};

	struct Block
	{
		uint id;
		Scope* scope;
		vector<std::unique_ptr<Statement>> statements;

		Block(Scope* scope)
			: scope(scope)
		{
			static uint blockId = 0;
			this->id = ++blockId;
		}

		template<typename T>
		T* addStatement(std::unique_ptr<T> statement)
		{
			T* ret = statement.get();
			ret->scopeLocalIndex = getCurrentScopeOrder(scope);
			this->statements.push_back(std::move(statement));
			return ret;
		}

		bool isEmpty()
		{
			return statements.empty();
		}
	};	

	struct Call : Statement, Expression
	{
		const TypeRef type;
		std::unique_ptr<Expression> callable;
		vector<std::unique_ptr<Expression>> args;

		Call(TypeRef& type) 
			: Statement(Statement::Call)
			, Expression(Expression::Call)
			, type(type)
		{}

		virtual string toString() const override
		{
			string s = "Call";
			return s;
		}

		virtual const TypeRef& getType() const override { return this->type; }

		void setCallable(std::unique_ptr<Expression> expr)
		{
			this->callable = std::move(expr);
		}

		void addArgument(std::unique_ptr<Expression> expr)
		{
			this->args.push_back(std::move(expr));
		}

		virtual const vector<Expression*> getSubExpressions() override
		{ 
			vector<Expression*> retVec = { &*callable };
			for (auto& a : args)
				retVec.push_back(&*a);
			return retVec; 
		}
	};

	struct Assignment : Statement
	{
		unique<Expression> assignable;
		unique<Expression> expression;

		Assignment(unique<Expression> assignable, unique<Expression> expression) 
			: Statement(Statement::Assignment)
			, assignable(std::move(assignable))
			, expression(std::move(expression))
		{}	
	};

	struct Return : Statement
	{
		unique<Expression> expr;
		Return(unique<Expression> expr)
			: Statement(Statement::Return)
			, expr(std::move(expr))
		{}

		Return()
			: Statement(Statement::Return)
		{}		
	};

	struct Scope : Statement
	{
		uint id;
		int statementCount = 0;
		vector<unique<Variable>> variables;
		vector<unique<Block>> blocks;

		vector<struct Defer*> deferStatements;		

		Scope()
			: Statement(Statement::Scope)
		{
			static uint scopeId = 0;
			this->id = ++scopeId;
		}

		bool isEmpty() const
		{
			return blocks.empty();
		}

		void trackDeferStatement(struct Defer* deferStatement)
		{
			this->deferStatements.push_back(deferStatement);
		}

		string getScopeBlockName(string suffix = "")
		{
			return string("scope") + std::to_string(id) + suffix;
		}

		Block* addBlock()
		{
			this->blocks.push_back(std::make_unique<IR::Block>(this));
			return this->blocks.back().get();
		}

		Variable* addVariable(unique<Variable> variable)
		{
			this->variables.push_back(std::move(variable));
			return this->variables.back().get();
		}
	};

	int getCurrentScopeOrder(Scope* scope)
	{
		return scope->statementCount++;
	}

	struct Conditional : Statement
	{
		unique<Expression> expr;
		struct Scope trueScope;
		struct Scope falseScope;

		Conditional(unique<Expression> expr) 
			: Statement(Statement::Conditional)
			, expr(std::move(expr))
		{}
	};

	struct Loop : Statement
	{
		struct Scope scope;
		Loop()
			: Statement(Statement::Loop)
		{}
	};

	struct Continue : Statement
	{
		Continue()
			: Statement(Statement::Continue)
		{}
	};

	struct Break : Statement
	{
		Break()
			: Statement(Statement::Break)
		{}
	};

	struct Defer : Statement
	{
		struct Scope scope;
		Defer()
			: Statement(Statement::Defer)
		{}
	};	

	// TODO: Param can only be of Variable type
	//	Should maybe inherit from Variable instead, but that means that
	//	referenceable probably cannot uniquely own their value, replace with pointer
	struct Param : Variable
	{
		int index = -1;
		using Variable::Variable;
	};

	struct Signature
	{
		vector<unique<Param>> inParams;
		vector<unique<Param>> outParams;

		Param* addInParam(const TypeRef& type, string name, SymbolSource* symbolSource)
		{
			this->inParams.push_back(std::make_unique<Param>(type, name, symbolSource));
			Param& param = *this->inParams.back();
			param.index = this->inParams.size() - 1;
			return &param;
		}

		Param* addOutParam(const TypeRef& type, string name, SymbolSource* symbolSource)
		{
			this->outParams.push_back(std::make_unique<Param>(type, name, symbolSource));
			Param& param = *this->outParams.back();
			param.index = this->outParams.size() - 1;
			return &param;
		}	
	};

	// A function can be used a literal, but since we should copy instruction memory, the 
	//	actual value of a function is its id
	using FunctionId = u64;
	static FunctionId functionId = 0;
	struct Function
	{
		FunctionId id;
		TypeRef type;
		string name; // Duplicate of constant name for non-main/lambdas
		Signature signature;
		Scope scope;
		Backend::Value* backendValue = nullptr;

		Function(const TypeRef type, string name)
			: id(++functionId)
			, type(type)
			, name(name)
		{	
		}

		Signature& getSignature()
		{
			return signature;
		}

		Scope& getScope()
		{
			return scope;
		}

		string getName()
		{
			return name;
		}

		FunctionId getId() const
		{
			return id;
		}

		const TypeRef& getType() const
		{
			return type;
		}

		unique<Literal> createLiteral()
		{
			return std::make_unique<IR::Literal>(type, vector<u8>((u8*)&id, ((u8*)&id) + sizeof(FunctionId)));
		}

		unique<Constant> createConstant(SymbolSource* symbolSource)
		{
			return std::make_unique<IR::Constant>(createLiteral(), name, symbolSource);
		}
	};

	struct Module
	{
		// Main functions are "lambdas" since they cannot be recerenced :)
		unique<Function> localMain;

		vector<unique<External>> externals;
		vector<unique<Constant>> constants;
		vector<unique<Function>> functions;
		vector<unique<Variable>> globals;	

		// TODO: Build the correct scopes directly from AST to avoid searching for symbols
		std::unordered_map<const SymbolSource*, Referenceable*> refMap;

		std::unordered_map<FunctionId, Function*> functionMap;	

		void cacheReferenceable(Referenceable* ref)
		{
			this->refMap[ref->symbolSource] = ref;
		}

		Referenceable* getReferenceable(SymbolSource* symbolSource)
		{
			auto* ref = refMap[symbolSource];
			return ref;
		}

		Function* addFunction(unique<Function> func)
		{
			functions.push_back(std::move(func));
			auto* funcPtr = &*functions.back();
			functionMap[funcPtr->getId()] = funcPtr;
			return funcPtr;
		}

		Function* getFunction(FunctionId id)
		{
			return functionMap[id];
		}

		Variable* addGlobal(unique<Variable> variable)
		{
			globals.push_back(std::move(variable));
			cacheReferenceable(&*globals.back());
			return &*globals.back();
		}

		Constant* addConstant(unique<Constant> constant)
		{
			constants.push_back(std::move(constant));
			cacheReferenceable(&*constants.back());
			return &*constants.back();
		}

		External* addExternal(unique<External> external)
		{
			externals.push_back(std::move(external));
			cacheReferenceable(&*externals.back());
			return &*externals.back();
		}

		External* getExternalByName(string name)
		{
			for (auto& e : externals)
			{
				if (e->name == name)
					return e.get();
			}

			return nullptr;
		}
	};
};

