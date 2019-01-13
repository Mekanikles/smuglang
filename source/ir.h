#pragma once

#include <unordered_map>
#include "core.h"
#include "types.h"
#include "backend/value.h"
#include "symbols.h"

// TODO: Rename to "MIR"? Mid-level intermediate representation
namespace IR
{
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
			BinaryOp,
			Call,
		};

		ExpressionType exprType;

		Expression(ExpressionType exprType) 
			: Value(Value::ValueType::Expression)
			, exprType(exprType) 
		{}
	
		virtual ~Expression() = default;
		virtual string toString() = 0;
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
			Eq,
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

		virtual string toString() override
		{
			switch (opType)
			{
				case Add: return "Add";
				case Sub: return "Sub";
				case Mul: return "Mul";
				case Div: return "Div";
				case Eq: return "Eq";
			}
			return "Unknown op";
		}

		virtual const vector<Expression*> getSubExpressions() override
		{ 
			return { &*leftExpr, &*rightExpr }; 
		}

		virtual const TypeRef& getType() const override { return this->type; }
	};

	struct Literal : Expression
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

		virtual string toString() override
		{
			string s = "Literal";
			return s;
		}

		virtual const TypeRef& getType() const override { return this->type; }
	};

	struct Referenceable : Value
	{
		enum class Type
		{
			Variable,
			External,
			Constant
		};

		Type type;
		string name;

		// TODO: Need this for mapping to sources, for now
		const SymbolSource* symbolSource;

		Referenceable(Type type, string name, const SymbolSource* symbolSource)
			: Value(Value::ValueType::Referenceable)
			, type(type)
			, name(name)
			, symbolSource(symbolSource)
		{}

		const string getName() const { return name; }
		virtual const TypeRef& getType() const = 0;

		const struct Constant& asConstant() const
		{
			assert(type == Type::Constant);
			return *(const struct Constant*)(this);
		}
	};

	struct Reference : Expression
	{
		const Referenceable* referenceable;

		Reference(const Referenceable* referenceable) 
			: Expression(Expression::Reference) 
			, referenceable(referenceable)
		{}

		virtual const Referenceable* getReferenceable() { return referenceable; }

		virtual string toString() override
		{
			return prettyString(referenceable->getName(), FGTextColor::Blue, true);
		}

		virtual const TypeRef& getType() const override { return referenceable->getType(); }			
	};

	struct Variable : Referenceable
	{
		const TypeRef type;	

		Variable(const TypeRef& type, string name, const SymbolSource* symbolSource) 
			: Referenceable(Referenceable::Type::Variable, name, symbolSource)
			, type(type)
		{}

		Variable(Variable&& var)
			: Referenceable(Referenceable::Type::Variable, std::move(var.name), var.symbolSource)
			, type(std::move(var.type))
		{
		}

		virtual const TypeRef& getType() const override { return type; }
	};

	struct External : Referenceable
	{
		const TypeRef type;

		External(const TypeRef type, string name, const SymbolSource* symbolSource)
			: Referenceable(Referenceable::Type::External, std::move(name), symbolSource)
			, type(type)
		{}

		virtual const TypeRef& getType() const override { return type; }	
	};

	struct Constant : Referenceable
	{
		unique<Literal> literal;

		Constant(unique<Literal> literal, string name, const SymbolSource* symbolSource) 
			: Referenceable(Referenceable::Type::Constant, std::move(name), symbolSource) 
			, literal(std::move(literal))
		{}

		Constant(Constant&& con)
			: Referenceable(Referenceable::Type::Constant, std::move(con.name), con.symbolSource)
			, literal(std::move(con.literal))
		{}

		virtual const TypeRef& getType() const override { return literal->getType(); }
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
			Return
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

	struct Conditional : Statement
	{
		unique<Expression> condExpr;
		Block trueBlock;
		Block falseBlock;

		Conditional() 
			: Statement(Statement::Conditional)
		{}
	};

	struct Return : Statement
	{
		unique<Expression> expr;
		Return(unique<Expression> expr)
			: Statement(Statement::Return)
			, expr(std::move(expr))
		{}
	};

	struct Scope : Statement
	{
		uint id;	
		vector<unique<Variable>> variables;
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

		Variable* addVariable(unique<Variable> variable)
		{
			this->variables.push_back(std::move(variable));
			return this->variables.back().get();
		}
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

	static u64 functionId = 0;
	struct Function : Literal
	{
		u64 id;
		string name; // Duplicate of constant name for non-main/lambdas
		Signature signature;
		Scope scope;

		Function(const TypeRef type, string name)
			: Literal(type, vector<u8> { (u8*)&id, ((u8*)&id) + sizeof(id) })
			, id(++functionId)
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

		u64 getId() const
		{
			return *(u64*)(data.data());
		}
	};

	struct Module
	{
		// Main is a "lambda" since it cannot be recerenced :)
		unique<Function> main;
		vector<unique<External>> externals;
		vector<unique<Constant>> constants;
		vector<Function*> functions;

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

		Constant* addFunction(unique<Function> func, SymbolSource* symbolSource)
		{
			functions.push_back(func.get());
			auto constant = std::make_unique<IR::Constant>(std::move(func), func->name, symbolSource);
			addConstant(std::move(constant));
			return &*constants.back();
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
	};
};

