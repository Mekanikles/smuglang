#pragma once
#include "core.h"

#include "llvm/ExecutionEngine/ExecutionEngine.h"
#include "llvm/ExecutionEngine/GenericValue.h"

#include "symbols.h"
#include "ast.h"
#include "context.h"
#include "ir.h"
#include "concretization.h"
#include "backend/backend.h"

struct EvaluationContext : ConcretizerContext
{
	EvaluationContext(Backend::Context* backend, IR::Module* module)
		: ConcretizerContext { backend, module }
	{}
};

namespace Evaluation
{

bool init(EvaluationContext& context)
{
	LLVMLinkInInterpreter();
	return true;
}

unique<IR::Expression> concretizeExpression(EvaluationContext& eContext, ASTContext& astContext, AST::Expression& expr)
{
	ExpressionConcretizer c(&eContext, &astContext); 
	expr.accept(&c);

	// TODO: Handle multiple return values
	assert(c.expressionStack.size() == 1);
	return std::move(c.expressionStack.back());
}

unique<IR::Literal> createLiteralFromConstantExpression(EvaluationContext& eContext, ASTContext& astContext, AST::Expression& expr)
{
	auto cexpr = concretizeExpression(eContext, astContext, expr);
	switch (cexpr->exprType)
	{
		case IR::Expression::Literal:
		{
			return unique<IR::Literal>(static_cast<IR::Literal*>(cexpr.release()));
		}
		case IR::Expression::Reference:
		{
			auto* ref = static_cast<IR::Reference*>(&*cexpr);
			assert(ref->referenceable);

			const IR::Constant& constant = ref->referenceable->asConstant();
			return std::make_unique<IR::Literal>(constant.getType(), constant.literal->data);
		}
		case IR::Expression::BinaryOp:
		{
		}
		case IR::Expression::Call:
		{
		}
	}

	assert("Could not create literal from constant expression" && false);
	return unique<IR::Literal>(nullptr);
}

void storeConstantFromExpression(EvaluationContext& eContext, ASTContext& astContext, AST::Expression& expr, SymbolSource& source)
{
	auto literal = createLiteralFromConstantExpression(eContext, astContext, expr);	
	auto constant = std::make_unique<IR::Constant>(std::move(literal), source.getSymbol()->name, &source);
	eContext.module->addConstant(std::move(constant));
}

string readStringFromLiteral(IR::Literal& literal)
{
	auto& type = literal.getType();
	if (!isStringType(type.getType()))
	{
		assert("Expression is not of string type" && false);
	}

	// TODO: handle other strings
	assert(type->isPointer());
	auto& innerType = type->getPointer().type;
	assert(innerType->isPrimitive());
	assert(innerType->getPrimitive().isChar());
	assert(innerType->getSize() == 8);

	string str = (const char*)literal.data.data();
	return str;
}

string evaluateExpressionAsString(EvaluationContext& eContext, ASTContext& astContext, AST::Expression& expr)
{
	auto literal = createLiteralFromConstantExpression(eContext, astContext, expr);	
	return readStringFromLiteral(*literal);
}

}