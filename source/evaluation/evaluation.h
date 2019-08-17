#pragma once
#include "core.h"

#include "llvm/ExecutionEngine/ExecutionEngine.h"
#include "llvm/ExecutionEngine/GenericValue.h"

#include "symbols.h"
#include "ast.h"
#include "context.h"
#include "ir/ir.h"
#include "ir/irutils.h"
#include "concretization.h"
#include "backend/backend.h"
#include "output.h"

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
	printLine(string("Concretizing expression at node ") + std::to_string(expr.order) + string("..."));
	ExpressionConcretizer c(&eContext, &astContext);
	auto irExpr = c.concretizeExpression(expr);
	printLine("  result:");
	printIRExpression(*(irExpr.get()), 1);
	return irExpr;
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
			case IR::Expression::ArrayAccess:
			{
				auto& arrAcc = static_cast<IR::ArrayAccess&>(expr);
				assert(arrAcc.baseExpr);
				assert(arrAcc.indexExpr);

				auto baseExprLiteral = evaluate(*arrAcc.baseExpr);
				auto indexExprLiteral = evaluate(*arrAcc.indexExpr);

				if (baseExprLiteral->getType()->isTypeVariable())
				{
					const i64 length = convertLiteralToInt64(*indexExprLiteral);

					auto& typeVar = arrAcc.baseExpr->getType()->getTypeVariable();
					TypeRef arrayType = createStaticArrayType(typeVar.type.clone(), length);
					return createTypeLiteral(createTypeVariable(std::move(arrayType)));
				}
				else
				{
					assert(false&& "ArrayAccess evaluation on non-types not supported yet");
				}				
			}			
			case IR::Expression::MemberAccess:
			{
				assert(false && "MemberAccess evaluation not supported yet");
			}
			case IR::Expression::BinaryOp:
			{
				assert(false && "BinaryOp evaluation not supported yet");
			}
			case IR::Expression::UnaryOp:
			{
				assert(false && "UnaryOp evaluation not supported yet");
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

void storeConstantFromLiteral(EvaluationContext& eContext, ASTContext& astContext, shared<IR::Literal> literal, SymbolSource& source)
{
	string name = source.getSymbol()->name;
	auto constant = std::make_unique<IR::Constant>(literal, name, &source);
	eContext.module->addConstant(std::move(constant));
}

void storeConstantFromExpression(EvaluationContext& eContext, ASTContext& astContext, AST::Expression& expr, SymbolSource& source)
{
	auto literal = createLiteralFromASTExpression(eContext, astContext, expr);
	storeConstantFromLiteral(eContext, astContext, std::move(literal), source);
}

void storeGlobal(EvaluationContext& eContext, const TypeRef& type, SymbolSource& source)
{
	createAndAddGlobal(eContext, type, source);
}

void storeExternal(EvaluationContext& eContext, const TypeRef& type, SymbolSource& source)
{
	createAndAddExternal(eContext, type, source);
}

IR::Function* createAndStoreFunctionHeader(EvaluationContext& eContext, ASTContext& astContext, AST::FunctionLiteral& literal, SymbolSource& source)
{
	string name = source.getSymbol()->name;
	auto& type = literal.getType(&astContext);
	auto func = std::make_unique<IR::Function>(type, name);
	auto funcPtr = eContext.module->addFunction(std::move(func));

	auto constant = std::make_unique<IR::Constant>(funcPtr->createLiteral(), name, &source);
	eContext.module->addConstant(std::move(constant));

	return funcPtr;
}

void generateFunctionBody(EvaluationContext& eContext, ASTContext& astContext, IR::Function& function, AST::FunctionLiteral& literal)
{
	generateConcreteFunction(&eContext, &astContext, function, &literal);
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