#pragma once

#include "core.h"
#include "types.h"
#include "backend.h"

namespace IR
{
	struct Variable
	{
		const TypeRef type;

		Variable(const TypeRef& type) 
			: type(type)
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

		virtual string toString() = 0;
		virtual const TypeRef& getType() = 0;
		virtual const vector<Expression*> getSubExpressions() { return {}; }
	};

	// TODO: Replace with generalized "value" with type+data?
	//	if so, how to handle constants in llvm?
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
			string s = "VariableRef";
			return s;
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
			Assignment,
			Call,
			Conditional,
		};

		StatementType statementType;

		Statement(StatementType statementType) 
			: statementType(statementType) 
		{}		
	};

	struct Block
	{
		vector<unique<Statement>> statements;
	};	

	struct Call : Statement, Expression
	{
		const TypeRef type;

		Call() 
			: Statement(Statement::Call)
			, Expression(Expression::Call) 
		{}

		virtual const TypeRef& getType() override
		{
			return type;
		}		
	};

	struct Assignment : Statement
	{
		VariableRef* var = nullptr;
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

	struct Function
	{
		string name;
		vector<Variable> variables;
		vector<Block> blocks;
		Block* entryBlock;

		Variable* addVariable(const TypeRef& type)
		{
			this->variables.push_back(IR::Variable(type));
			Variable& var = this->variables.back();
			return &var;
		}
	};

	struct Module
	{
		vector<Function> functions;
		Function mainFunction;
	};
};

