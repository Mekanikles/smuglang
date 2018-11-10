#pragma once

#include "core.h"
#include "types.h"
#include "backend.h"

// TODO: Rename to "MIR"? Mid-level intermediate representation
namespace IR
{
	struct Variable
	{
		const TypeRef type;
		string name;

		// TODO: :(
		DeclarationSymbolSource* symbolSource;

		Variable(const TypeRef& type, string name, DeclarationSymbolSource* symbolSource) 
			: type(type)
			, name(name)
			, symbolSource(symbolSource)
		{}
	};

	struct Expression
	{
		enum ExpressionType
		{
			VariableRef,
			Call,
			Value,
		};

		ExpressionType exprType;

		Expression(ExpressionType exprType) 
			: exprType(exprType) 
		{}
	
		virtual ~Expression() = default;

		virtual string toString() = 0;
		virtual const TypeRef& getType() = 0;
		virtual const vector<Expression*> getSubExpressions() { return {}; }
	};

	struct Value : Expression
	{
		const TypeRef type;
		BackendValue* value;

		Value(const TypeRef type, BackendValue* value)
			: Expression(Expression::Value)
			, type(type)
			, value(value)
		{}

		virtual string toString() override
		{
			string s = "Value";
			return s;
		}

		virtual const TypeRef& getType() override
		{
			return type;
		}
	};

	struct VariableRef : Expression
	{
		const Variable* variable;

		VariableRef(const Variable* variable) 
			: Expression(Expression::VariableRef) 
			, variable(variable)
		{}

		virtual string toString() override
		{
			return prettyString(variable->name, FGTextColor::Blue, true);
		}

		virtual const TypeRef& getType() override
		{
			return variable->type;
		}
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

		virtual const TypeRef& getType() override
		{
			return type;
		}

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
		unique<VariableRef> var;
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
		vector<Variable> variables;
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

		Variable* addVariable(const TypeRef& type, string name, DeclarationSymbolSource* symbolSource)
		{
			this->variables.push_back(IR::Variable(type, name, symbolSource));
			Variable& var = this->variables.back();
			return &var;
		}
	};

	struct Function
	{
		uint id;
		const TypeRef type;	
		string name;
		Scope scope;

		Function(const TypeRef& type, string name) 
			: type(type)
			, name(name)
		{
			static uint functionId = 0;
			this->id = ++functionId;
		}
	};

	struct Module
	{
		vector<unique<Function>> functions;
		Function* mainFunction = nullptr;

		// TODO: Build the correct scopes directly from AST to avoid searching for vars
		std::unordered_map<SymbolSource*, Variable*> variableMap;

		void cacheVariable(Variable* var)
		{
			this->variableMap[var->symbolSource] = var;
		}

		Variable* getVariable(SymbolSource* symbolSource)
		{
			auto* var = this->variableMap[symbolSource];
			assert(var);
			return var;
		}
	};
};

