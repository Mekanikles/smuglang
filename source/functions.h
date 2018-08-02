

struct FunctionArgumentBinding
{
	struct Param
	{
		TypeRef type;
		int index;

		Param(TypeRef&& type, int index) : type(std::move(type)), index(index) {}
	};

	vector<Param> params;
};

FunctionArgumentBinding* createFunctionArgumentBinding(const AST::Call* callNode, const FunctionClass& functionClass)
{
	auto* binding = createObject<FunctionArgumentBinding>();

	const auto& functionInParams = functionClass.inParams;
	const auto& args = callNode->args;

	const int argCount = args.size();
	int argsConsumed = 0;
	// TODO: For now, we only allow left-to-right consuming of args
	//	tuples must be explicitly provided.
	// In the future, we want to allow fleible mapping between single args
	//	and tuples, plus named argument
	for (const auto& p : functionInParams)
	{
		const TypeRef& type = p.type;
		if (argsConsumed >= argCount)
			assert(false && "Too few arguments to function");

		// Important to clone type so we can do type inference for each
		//	call rather than the function
		binding->params.emplace_back(type.clone(), argsConsumed++);
	}

	if (argsConsumed != argCount)
		assert(false && "Too many arguments to function");

	// TODO: Bind to output params

	return binding;
}

bool unifyArguments(AST::Call* callNode, FunctionArgumentBinding* argBinding)
{
	auto& args = callNode->args;
	for (auto& p : argBinding->params)
	{
		TypeRef& t1 = args[p.index]->getType();
		TypeRef& t2 = p.type;

		const auto result = unifyTypes(t1, t2);
		if (result == CannotUnify)
		{
			assert(false && "Could not unify function argument");
			return false;
		}
	}

	return true;
}















