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

struct ExpressionEvaluator
{
	unique<IR::Literal> evaluateCall(IR::Function& func)
	{
		for (auto& block : func.scope.blocks)
		{
			for (auto& statement : block->statements)
			{
				switch (statement->statementType)
				{
					case IR::Statement::Return:
					{
						IR::Return& ret = static_cast<IR::Return&>(*statement);
						return evaluate(*ret.expr);
					}
					default:
					{
						assert(false && "Call evaluation does not support much yet...");
					}
				}
			}
		}

		assert(false && "No return value found!");
		return unique<IR::Literal>();
	}

	// TODO: Add "Value" object? Should not use literal for this
	unique<IR::Literal> evaluate(IR::Expression& expr)
	{
		switch (expr.exprType)
		{
			case IR::Expression::Literal:
			{
				auto& literal = static_cast<IR::Literal&>(expr);
				return literal.copy();
			}
			case IR::Expression::Reference:
			{
				auto& ref = static_cast<IR::Reference&>(expr);
				assert(ref.referenceable);

				assert(ref.referenceable->isConstant() && "Only constant references are supported for now");

				const IR::Constant& constant = ref.referenceable->asConstant();
				return std::make_unique<IR::Literal>(constant.getType(), constant.literal->data);
			}
			case IR::Expression::BinaryOp:
			{
				assert(false && "BinaryOp evaluation not supported yet");
			}
			case IR::Expression::Call:
			{
				auto& call = static_cast<IR::Call&>(expr);
				assert(call.callable.get());
				auto callable = evaluate(*call.callable.get());
				assert(call.args.empty() && "Evaluation of call arguments not supported yet");

				IR::Function* func = eContext.module->getFunction(callable->readValue<IR::FunctionId>());
				assert(func);

				return evaluateCall(*func);
			}
		}

		assert(false && "Could not evaluate unknown expression");
	}

	ExpressionEvaluator(EvaluationContext& eContext)
		: eContext(eContext)
	{
	}

	EvaluationContext& eContext;
};

unique<IR::Literal> createLiteralFromASTExpression(EvaluationContext& eContext, ASTContext& astContext, AST::Expression& expr)
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
		default:
		{
			ExpressionEvaluator e(eContext);
			return e.evaluate(*cexpr.get());
		}
	}
}

void storeConstantFromExpression(EvaluationContext& eContext, ASTContext& astContext, AST::Expression& expr, SymbolSource& source)
{
	string name = source.getSymbol()->name;
	auto literal = createLiteralFromASTExpression(eContext, astContext, expr);
	auto constant = std::make_unique<IR::Constant>(std::move(literal), name, &source);
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
	auto literal = createLiteralFromASTExpression(eContext, astContext, expr);	
	return readStringFromLiteral(*literal);
}

}