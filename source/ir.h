#pragma once

#include "core.h"
#include "types.h"
#include "backend/value.h"

// TODO: Rename to "MIR"? Mid-level intermediate representation
namespace IR
{
	struct Value
	{
		enum ValueType
		{
			Variable,
			Expression,
			Function, // TODO: Technically an expression literal?
		};

		ValueType valueType;
		Backend::Value* backendValue = nullptr;

		Value(ValueType valueType)
			: valueType(valueType)
		{}
		virtual const TypeRef& getType() const = 0;
	};

	struct Referenceable
	{
		string name;

		// TODO: Need this for mapping to sources, for now
		const SymbolSource* symbolSource;

		Referenceable(string name, const SymbolSource* symbolSource, unique<Value> value)
			: name(name)
			, symbolSource(symbolSource)
			, value(std::move(value))
		{}
		
		unique<Value> value;

		const string getName() const { return name; }
		const TypeRef& getType()  const { return value->getType(); }
	};

	struct Variable : Value
	{
		const TypeRef type;	

		Variable(const TypeRef& type) 
			: Value(Value::Variable)
			, type(type)
		{}

		Variable(Variable&& var)
			: Value(std::move(var))
			, type(std::move(var.type))
		{
		}

		virtual const TypeRef& getType() const override { return type; }
	};

	struct Expression : Value
	{
		enum ExpressionType
		{
			Reference,
			Call,
			Literal,
		};

		ExpressionType exprType;

		Expression(ExpressionType exprType) 
			: Value(Value::Expression)
			, exprType(exprType) 
		{}
	
		virtual ~Expression() = default;
		virtual string toString() = 0;
		virtual const vector<Expression*> getSubExpressions() { return {}; }
	};

	struct Literal : Expression
	{
		const TypeRef type;

		Literal(const TypeRef type, Backend::Value* backendValue)
			: Expression(Expression::Literal)
			, type(type)
		{
			this->backendValue = backendValue;
		}

		virtual string toString() override
		{
			string s = "Value";
			return s;
		}

		virtual const TypeRef& getType() const override { return this->type; }		
	};

	struct Reference : Expression
	{
		const Referenceable* referenceable;

		Reference(const Referenceable* referenceable) 
			: Expression(Expression::Reference) 
			, referenceable(referenceable)
		{}

		virtual string toString() override
		{
			return prettyString(referenceable->getName(), FGTextColor::Blue, true);
		}

		virtual const TypeRef& getType() const override { return referenceable->getType(); }			
	};

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
		};

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
		vector<std::unique_ptr<Statement>> statements;

		Block()
		{
			static uint blockId = 0;
			this->id = ++blockId;
		}

		template<typename T>
		T* addStatement(std::unique_ptr<T> statement)
		{
			T* ret = statement.get();
			this->statements.push_back(std::move(statement));
			return ret;
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

		virtual string toString() override
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
	};

	struct Assignment : Statement
	{
		unique<Reference> ref;
		unique<Expression> expression;

		Assignment() 
			: Statement(Statement::Assignment)
		{}	
	};

	struct Conditional : Statement
	{
		unique<Expression> condExpr;
		Block trueBlock;
		Block falseBlock;

		Conditional() 
			: Statement(Statement::Conditional)
		{}
	};

	struct Scope : Statement
	{
		uint id;	
		vector<unique<Referenceable>> referenceables;
		vector<unique<Block>> blocks;

		Scope()
			: Statement(Statement::Scope)
		{
			static uint scopeId = 0;
			this->id = ++scopeId;
		}

		Block* addBlock()
		{
			this->blocks.push_back(std::make_unique<IR::Block>());
			return this->blocks.back().get();
		}

		Referenceable* addReferenceable(string name, const SymbolSource* symbolSource, unique<Value> value)
		{
			this->referenceables.push_back(std::make_unique<Referenceable>(name, symbolSource, std::move(value)));
			return this->referenceables.back().get();
		}
	};

	// TODO: Param can only be of Variable type
	//	Should maybe inherit from Variable instead, but that means that
	//	referenceable probably cannot uniquely own their value, replace with pointer
	struct Param : Referenceable
	{
		int index = -1;
		using Referenceable::Referenceable;
	};

	struct Signature
	{
		vector<Param> inParams;
		vector<Param> outParams;

		Param* addInParam(const TypeRef& type, string name, SymbolSource* symbolSource)
		{
			auto var = std::make_unique<Variable>(type);
			this->inParams.push_back(Param(name, symbolSource, std::move(var)));
			Param& param = this->inParams.back();
			param.index = this->inParams.size() - 1;
			return &param;
		}

		Param* addOutParam(const TypeRef& type, string name, SymbolSource* symbolSource)
		{
			auto var = std::make_unique<Variable>(type);
			this->outParams.push_back(Param(name, symbolSource, std::move(var)));
			Param& param = this->outParams.back();
			param.index = this->outParams.size() - 1;
			return &param;
		}	
	};

	struct Function : Value
	{
		uint id;
		const TypeRef type;	
		string name;
		bool external;
		Signature signature;
		Scope scope;
		bool isVariadic = false;

		Function(const TypeRef& type, string name, bool external = false) 
			: Value(Value::Function)
			, type(type)
			, name(name)
			, external(external)
		{
			static uint functionId = 0;
			this->id = ++functionId;
		}

		virtual const TypeRef& getType() const override
		{
			return type;
		}		
	};

	struct Module
	{
		unique<Function> mainFunction; 

		// TODO: Build the correct scopes directly from AST to avoid searching for symbols
		std::unordered_map<const SymbolSource*, Referenceable*> refMap;

		void cacheReferenceable(Referenceable* ref)
		{
			this->refMap[ref->symbolSource] = ref;
		}

		Referenceable* getReferenceable(SymbolSource* symbolSource)
		{
			auto* ref = this->refMap[symbolSource];
			assert(ref);
			return ref;
		}
	};
};

