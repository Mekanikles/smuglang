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
T* createExpression(Args... args)
{
	T* n = createObject<T>(std::forward<Args>(args)...);
	return n;
}

struct ConcretizerContext
{
	Backend* backend;
	IR::Module* module;
};

struct ExpressionConcretizer : AST::Visitor
{
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
		currentBlock = block;
	}

	ConcretizerContext* context;
	IR::Function* function;
	IR::Block* currentBlock;
	Context* astContext;
	vector<IR::Expression*> expressionStack;
};

// TODO: StatementConcretizer?
struct FunctionConcretizer : AST::Visitor
{
	virtual void visit(AST::FunctionDeclaration* node) 
	{
		generateConcreteFunction(node->funcLiteral, node->symbol);
	}

	virtual void visit(AST::SymbolDeclaration* node) 
	{
		auto* var = this->function->addVariable(node->getType(this->astContext));

		if (node->initExpr)
		{
			auto* assignment = createStatement<IR::Assignment>();
			assignment->var = createExpression<IR::VariableRef>(var);
			
			vector<IR::Expression*> expressions = generateConcreteExpression(node->initExpr);			
		} 
	}

	vector<IR::Expression*> generateConcreteExpression(AST::Expression* expression)
	{
		ExpressionConcretizer c(this->context, this->astContext, this->function, this->currentBlock); 
		expression->accept(&c);
		return std::move(c.expressionStack);
	}

	IR::Function* generateConcreteFunction(AST::FunctionLiteral* funcLiteral, string name)
	{
		auto* module = this->context->module;
		module->functions.push_back(IR::Function());
		IR::Function& func = module->functions.back();
		func.name = name;
		FunctionConcretizer c(this->context, this->astContext, &func); 
		funcLiteral->accept(&c);
		return &func;
	}

	FunctionConcretizer(ConcretizerContext* context, Context* astContext, IR::Function* function)
		: context(context)
		, function(function)
		, astContext(astContext)
	{
		function->blocks.push_back(IR::Block());
		currentBlock = &function->blocks.back();
		function->entryBlock = currentBlock;
	}

	ConcretizerContext* context;
	IR::Function* function;
	IR::Block* currentBlock;
	Context* astContext;
};

IR::Module concretizeAST(Backend* backend, Context* astContext, AST::ASTObject* ast)
{
	assert(ast->root);
	IR::Module module;	
	ConcretizerContext context { backend, &module };
	module.mainFunction.name = "main";
	FunctionConcretizer c(&context, astContext, &module.mainFunction);
	ast->root->accept(&c);

	return module;
}








 