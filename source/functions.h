#pragma once

#include "core.h"
#include "types.h"
#include "context.h"
#include "ast.h"

struct ArgumentBinding
{
	struct Param
	{
		int paramIndex;
		int argIndex;
		int argCount;

		// Hm, this will be filled in by unification
		TypeRef type;

		bool isVariadic()
		{
			return argCount > 1;
		}

		Param(int paramIndex, int argIndex, int argCount = 1) 
			: paramIndex(paramIndex)
			, argIndex(argIndex) 
			, argCount(argCount)
		{}
	};

	vector<Param> params;
};

struct ArgProvider
{
	struct Arg
	{
		string name;
		AST::Expression* expr;
	};

	vector<Arg> args;
	int consumed = 0;

	bool canConsume()
	{
		return consumed < args.size();
	}

	int consume()
	{
		assert(canConsume() && "Too few arguments to function");
		return consumed++;
	}
};

struct ParamProvider
{
	struct Param
	{
		string name;
		bool isVariadic;
	};

	vector<Param> params;
	int consumed = 0;

	bool canConsume()
	{
		return consumed < params.size();
	}

	int consume()
	{
		assert(canConsume() && "Too few arguments to function");
		return consumed++;
	}

	bool isVariadic()
	{
		assert(consumed < params.size());
		return params[consumed].isVariadic;
	}
};

ArgumentBinding* createArgumentBinding(ArgProvider& args, ParamProvider& params)
{
	auto* binding = createObject<ArgumentBinding>();

	// TODO: For now, we only allow left-to-right consuming of args
	//	tuples must be explicitly provided.
	// In the future, we want to allow fleible mapping between single args
	//	and tuples, plus named argument
	while (params.canConsume())
	{
		if (params.isVariadic())
		{
			// Eat rest of args
			int argIndex = args.consumed;
			int count = 0;
			while (args.canConsume())
			{
				args.consume();
				count++;
			}
			
			binding->params.push_back(ArgumentBinding::Param(params.consumed, argIndex, count));
		}
		else
		{
			binding->params.emplace_back(ArgumentBinding::Param(params.consumed, args.consumed));
			args.consume();
		}

		params.consume();
	}

	if (args.canConsume())
		assert(false && "Too many arguments for binding");

	return binding;
}

ArgumentBinding* createFunctionArgumentBinding(const AST::Call& callNode, const FunctionClass& function)
{
	ArgProvider args;
	int argCount = 0;
	for (auto* expr : callNode.args)
	{
		args.args.push_back(ArgProvider::Arg{ string("Arg ") + std::to_string(argCount), expr });
		argCount++;
	}
	
	ParamProvider params;
	for (auto& inParam : function.inParams)
	{
		params.params.push_back(ParamProvider::Param{ inParam.identifier, inParam.type->isTuple() });
	}

	if (function.isCVariadic)
		params.params.push_back(ParamProvider::Param{ "__cvarargs", true });

	return createArgumentBinding(args, params);
}

ArgumentBinding* createTemplateArgumentBinding(const AST::SymbolExpression& expr, const AST::TemplateDeclaration& templDecl)
{
	ArgProvider args;
	for (auto* expr : expr.templateArgs)
	{
		args.args.push_back(ArgProvider::Arg{ "", expr });
	}
	
	ParamProvider params;
	for (auto* inParam : templDecl.signature->inParams)
	{
		params.params.push_back(ParamProvider::Param{ "", inParam->isVariadic });
	}

	return createArgumentBinding(args, params);
}

UnificationResult createArgumentUnification(vector<TypeRef>& argTypes, vector<TypeRef>& paramTypes, ArgumentBinding* argBinding)
{
	UnificationResult result;
	for (auto& boundParam : argBinding->params)
	{
		if (boundParam.argCount == 0)
			continue;

		assert(boundParam.paramIndex < paramTypes.size());
		TypeRef& paramType = paramTypes[boundParam.paramIndex];

		if (boundParam.isVariadic())
		{
			vector<TypeRef> types;
			for (int i = boundParam.argIndex; i < boundParam.argIndex + boundParam.argCount; i++)
			{
				assert(i < argTypes.size());
				types.push_back(argTypes[i]);
			}

			boundParam.type = createTupleType(std::move(types));
		}
		else
		{
			assert(boundParam.argIndex < argTypes.size());
			boundParam.type = argTypes[boundParam.argIndex];	
		} 

		auto paramResult = generateTypeUnification(boundParam.type, paramType);
		if (!paramResult)
			return UnificationResult();

		result.merge(std::move(paramResult));
	}

	return result;
}

bool unifyArguments(vector<TypeRef>& argTypes, vector<TypeRef>& paramTypes, ArgumentBinding* argBinding)
{
	auto result = createArgumentUnification(argTypes, paramTypes, argBinding);
	if (!result)
		return false;
	result.apply();	
	return true;
}

bool unifyFunctionCall(ASTContext* context, AST::Call* call, const FunctionClass& function, ArgumentBinding* argBinding)
{
	vector<TypeRef> argTypes;
	for (AST::Expression* expr : call->args)
	{
		argTypes.push_back(expr->getType(context));
	}

	vector<TypeRef> paramTypes;
	for (const FunctionClass::Param& param : function.inParams)
	{
		// Important to clone all types so we can do type inference for each
		//	binding rather than the function itself		
		paramTypes.emplace_back(param.type.clone());
	}

	if (function.isCVariadic)
		paramTypes.emplace_back(createTupleType(TypeRef()));

	return unifyArguments(argTypes, paramTypes, argBinding);
}

bool unifyTemplateArguments(ASTContext* argContext, ASTContext* instanceContext, AST::SymbolExpression* expr, AST::FunctionSignature* signature, ArgumentBinding* argBinding)
{
	vector<TypeRef> argTypes;
	for (AST::Expression* expr : expr->templateArgs)
	{
		argTypes.push_back(expr->getType(argContext));
	}

	vector<TypeRef> paramTypes;
	for (AST::FunctionInParam* param : signature->inParams)
	{
		// Important to clone all types so we can do type inference for each
		//	binding rather than the function itself		
		paramTypes.emplace_back(param->getType(instanceContext));
	}

	return unifyArguments(argTypes, paramTypes, argBinding);
}













