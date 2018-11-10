#pragma once
#include "core.h"
#include "ir.h"
#include "ast.h"
#include "context.h"
#include "backend.h"

template<typename T, typename... Args>
T* createStatement(Args... args)
{
	T* n = createObject<T>(std::forward<Args>(args)...);
	return n;
}

template<typename T, typename... Args>
std::unique_ptr<T> createExpression(Args... args)
{
	T* n = createObject<T>(std::forward<Args>(args)...);
	return std::unique_ptr<T>(n);
}

struct ConcretizerContext
{
	Backend* backend;
	IR::Module* module;
};

struct ExpressionConcretizer : AST::Visitor
{
	virtual void visit(AST::SymbolExpression* node) 
	{
		SymbolDependency* symDep = this->astContext->getSymbolDependency(node);
		auto* source = symDep->source;

		IR::Variable* var = this->context->module->getVariable(source);
		assert(var);

		expressionStack.push_back(createExpression<IR::VariableRef>(var));
	}

	virtual void visit(AST::IntegerLiteral* node) 
	{
		TypeRef& type = node->getType(this->astContext);
		// TODO: Does support for default types go here?
		assert(type.getType().isConcrete());
		assert(type.getType().isPrimitive());
		const PrimitiveClass& primitive = type.getType().getPrimitive();
		assert(primitive.primitiveType == PrimitiveClass::Int);

		auto* value = this->context->backend->createIntegerValueFromText(node->value, primitive.size, primitive.signedType == PrimitiveClass::Signed);
		expressionStack.push_back(createExpression<IR::Value>(type, value));
	}
		
	ExpressionConcretizer(ConcretizerContext* context, Context* astContext, IR::Function* function, IR::Block* block)
		: context(context)
		, function(function)
		, astContext(astContext)
	{
		this->currentBlock = block;
	}

	ConcretizerContext* context;
	IR::Function* function;
	IR::Block* currentBlock;
	Context* astContext;
	vector<std::unique_ptr<IR::Expression>> expressionStack;
};

// TODO: StatementConcretizer?
struct FunctionConcretizer : AST::Visitor
{
	virtual void visit(AST::FunctionDeclaration* node) 
	{
		generateConcreteFunction(node->funcLiteral, node->symbol);
	}

	void addVariablesFromSymbolScope(IR::Scope* scope, SymbolScope* symbolScope)
	{
		assert(symbolScope);

		for (DeclarationSymbolSource* symbolSource : symbolScope->getDeclarations())
		{
			Symbol* symbol = symbolSource->symbol;
			assert(symbol);
			scope->addVariable(symbol->type, symbol->name, symbolSource);
		}

		// Record variables for lookup later
		for (auto& var : scope->variables)
			this->context->module->cacheVariable(&var);
	}

	void generateConcreteStatementBody(IR::Scope* scope, AST::StatementBody* node)
	{
		SymbolScope* symbolScope = this->astContext->getScope(node);
		addVariablesFromSymbolScope(scope, symbolScope);

		auto* prevBlock = this->currentBlock;
		this->currentBlock = scope->addBlock();
		AST::visitChildren(node, this);
		this->currentBlock = prevBlock;
	}

	virtual void visit(AST::StatementBody* node) 
	{
		auto* scope = this->currentBlock->addStatement(std::make_unique<IR::Scope>());
		
		generateConcreteStatementBody(scope, node);
	}

	virtual void visit(AST::Call* node) 
	{
		IR::Call* call = new IR::Call(node->getType(this->astContext));

		auto exprs = generateConcreteExpression(node->expr);
		assert(exprs.size() == 1);
		call->setCallable(std::move(exprs.back()));

		for (auto* arg : node->args)
		{
			auto exprs = generateConcreteExpression(arg);
			for (auto& expr : exprs)
			{
				call->addArgument(std::move(expr));
			}
		}

		this->currentBlock->addStatement(std::unique_ptr<IR::Statement>(call));
	}	

	vector<std::unique_ptr<IR::Expression>> generateConcreteExpression(AST::Expression* expression)
	{
		ExpressionConcretizer c(this->context, this->astContext, this->function, this->currentBlock); 
		expression->accept(&c);
		return std::move(c.expressionStack);
	}

	IR::Function* generateConcreteFunction(AST::FunctionLiteral* funcLiteral, string name)
	{
		auto* module = this->context->module;
		module->functions.push_back(std::make_unique<IR::Function>(funcLiteral->getType(this->astContext), name));
		IR::Function& func = *module->functions.back();
		FunctionConcretizer c(this->context, this->astContext, &func); 

		// Handle signature
		SymbolScope* symbolScope = this->astContext->getScope(funcLiteral->signature);
		c.addVariablesFromSymbolScope(&func.scope, symbolScope);

		// Handle body
		c.generateConcreteStatementBody(&func.scope, funcLiteral->body);
		return &func;
	}

	FunctionConcretizer(ConcretizerContext* context, Context* astContext, IR::Function* function)
		: context(context)
		, function(function)
		, astContext(astContext)
	{
	}

	ConcretizerContext* context = nullptr;
	IR::Function* function = nullptr;
	IR::Block* currentBlock = nullptr;
	Context* astContext = nullptr;
};

TypeRef createMainType()
{
	return createFunctionType();
}

IR::Module concretizeASTModule(Backend* backend, Context* astContext, AST::Module* astModule)
{
	assert(astModule);
	IR::Module module;
	ConcretizerContext context { backend, &module };

	module.functions.push_back(std::make_unique<IR::Function>(createMainType(), "main"));
	IR::Function& func = *module.functions.back();	
	module.mainFunction = &func;

	FunctionConcretizer c(&context, astContext, &func);
	c.generateConcreteStatementBody(&func.scope, astModule->body);
 
	return module;
}








 