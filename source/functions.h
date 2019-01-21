#pragma once

struct FunctionArgumentBinding
{
	struct Param
	{
		TypeRef type;
		int argIndex;
		int argCount;

		Param(TypeRef&& type, int argIndex, int argCount) 
			: type(std::move(type))
			, argIndex(argIndex) 
			, argCount(argCount)
		{}
	};

	vector<Param> params;
};

FunctionArgumentBinding* createFunctionArgumentBinding(const AST::Call* callNode, const FunctionClass& functionClass)
{
	auto* binding = createObject<FunctionArgumentBinding>();

	const auto& functionInParams = functionClass.inParams;

	struct ArgConsumer
	{
		const vector<AST::Expression*>& args;
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
	} args { callNode->args };

	// TODO: For now, we only allow left-to-right consuming of args
	//	tuples must be explicitly provided.
	// In the future, we want to allow fleible mapping between single args
	//	and tuples, plus named argument
	for (const auto& p : functionInParams)
	{
		// Important to clone all types so we can do type inference for each
		//	binding rather than the function itself	
		if (p.type->isTuple())
		{
			auto& tuple = p.type->getTuple();
			if (tuple.unbounded)
			{
				// Eat rest of args
				auto& elType = tuple.types.back();
				vector<TypeRef> types;
				int index = args.consumed;
				while (args.canConsume())
				{
					args.consume();
					types.push_back(elType.clone());
				}
				int count = types.size();			

				auto tupleType = createTupleType(std::move(types));
				binding->params.emplace_back(TypeRef(std::move(tupleType)), index, count);
			}
			else
			{
				vector<TypeRef> types;
				int index = args.consumed;
				for (auto& t : tuple.types)
				{
					types.push_back(t.clone());
				}
				int count = types.size();

				auto tupleType = createTupleType(std::move(types));
				binding->params.emplace_back(TypeRef(std::move(tupleType)), index, count);
			}		
		}
		else
		{
			int index = args.consume();
			binding->params.emplace_back(p.type.clone(), index, 1);
		}
	}

	if (args.canConsume())
		assert(false && "Too many arguments to function");

	// TODO: Bind to output params

	return binding;
}

bool unifyArguments(ASTContext* context, AST::Call* callNode, FunctionArgumentBinding* argBinding)
{
	auto& args = callNode->args;
	for (auto& p : argBinding->params)
	{
		if (p.argCount == 0)
			continue;

		TypeRef& paramType = p.type;
		TypeRef argType;
		if (paramType->isTuple())
		{
			// Wrap the argument types in a tuple, to be able to unify all at once
			// 	note that we do not clone the types here, we want to ref the args directly
			vector<TypeRef> types;
			for (int i = p.argIndex; i < p.argIndex + p.argCount; i++)
			{
				types.push_back(args[p.argIndex]->getType(context));
			}

			argType = createTupleType(std::move(types));
		}
		else
		{
			argType = args[p.argIndex]->getType(context);	
		} 

		const auto result = unifyTypes(argType, paramType);
		if (result == CannotUnify)
			assert(false && "Could not unify function argument");
	}

	return true;
}















