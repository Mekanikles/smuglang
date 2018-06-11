
// Processing is done through dependency chain, rather than AST order
struct ASTProcessor : AST::Visitor
{
	void visit(AST::Call* node) override
	{
		if (node->processed)
			return;
		node->processed = true;

		// Make sure function expression is processed
		node->expr->accept(this);

		Type& functionType = node->expr->getType();
		assert(functionType.isFunction());
		FunctionClass& function = functionType.getFunction();

		FunctionArgumentBinding* argBinding = createFunctionArgumentBinding(node, function);
		node->argBinding = argBinding;

		unifyArguments(node, argBinding);

		/*
		auto& inTypes = function.inTypes;
		auto& args = node->args;

		if (function.isVariadic)
			assert(args.size() >= inTypes.size());
		else
 			assert(inTypes.size() == args.size());

		for (int i = 0, s = inTypes.size(); i < s; ++i)
		{
			Type& inType = inTypes[i];
			AST::Expression* arg = args[i];
			Type& argType = arg->getType();
			const auto result = unifyTypes(argType, inType);

			// TODO: Handle implicit casts?
			if (result == CannotUnify)
				assert("Cannot unify function argument" && false);	
		}*/	
	}

	// TODO: Remove functiondeclaration special node, use symboldeclaration instead
	void visit(AST::FunctionDeclaration* node) override
	{
		if (node->processed)
			return;
		node->processed = true;

		assert(node->funcLiteral);
		// Process literal
		node->funcLiteral->accept(this);

		Type& functionType = node->funcLiteral->getType();

		Symbol* symbol = node->getSymbol();
		symbol->firstInitOrder = node->order;
		symbol->type = functionType;
	}

	void visit(AST::FunctionLiteral* node) override
	{
		if (node->processed)
			return;
		node->processed = true;

		assert(node->signature);
		assert(node->body);

		auto& paramScope = node->body->scope;

		node->signature->accept(this);
		AST::FunctionSignature* signature = node->signature;

		// Assign types to params
		for (AST::FunctionInParam* param : signature->inParams)
		{
			SymbolSource* symbolSource = paramScope.lookUpSymbolSource(param->name);
			if (!symbolSource)
				assert(false && "could not find param in scope");

			const bool isVariadic = param->isVariadic;
			Symbol* symbol = symbolSource->getSymbol();

			Type type;
			if (param->typeExpr)
			{
				const Type& exprType = param->typeExpr->getType();
				type = exprType.innerTypeFromTypeVariable();
			}

			// Infer type from init expression
			if (param->initExpr && !isVariadic)
			{
				symbol->firstInitOrder = node->order;
				if (param->initExpr)
				{
					Type& exprType = param->initExpr->getType();
					const auto result = unifyTypes(symbol->type, exprType);

					// TODO: Handle implicit casts?
					if (result == CannotUnify)
						assert("Cannot unify types" && false);

					// TODO: How to apply unification to expression?
				}
			}

			if (isVariadic)
				symbol->type = createTupleType(type);
			else
				symbol->type = type;
		}
		// TODO: outparams

		node->body->accept(this);

		// Now we can figure out the types in the signature
		//	and construct the complete function type
		FunctionClass& func = node->getType().getFunction();
		assert(func.inTypes.empty());
		assert(func.outTypes.empty());

		for (AST::FunctionInParam* param : signature->inParams)
		{
			SymbolSource* symbolSource = paramScope.lookUpSymbolSource(param->name);
			if (!symbolSource)
				assert(false && "could not find param in scope");

			Symbol* symbol = symbolSource->getSymbol();
			func.inTypes.push_back(symbol->type);
		}
		// TODO: outparams
	}

	void visit(AST::SymbolDeclaration* node) override
	{
		if (node->processed)
			return;
		node->processed = true;

		Symbol* symbol = node->getSymbol();

		// Check explicit type
		// TODO: How to assign "type" if inner type is always transferred?
		//	i.e var x : type; "type" needs to be a type variable that
		//	has an inner type of a type variable?
		if (node->typeExpr)
		{
			node->typeExpr->accept(this);
			const Type& type = node->typeExpr->getType();
			symbol->type = type.innerTypeFromTypeVariable();
		}

		// Infer type from init expression
		if (node->initExpr || node->isParam)
		{
			symbol->firstInitOrder = node->order;
			if (node->initExpr)
			{
				node->initExpr->accept(this);
				Type& exprType = node->initExpr->getType();
				const auto result = unifyTypes(symbol->type, exprType);

				// TODO: Handle implicit casts?
				if (result == CannotUnify)
					assert("Cannot unify types" && false);

				// TODO: How to apply unification to expression?
			}
		}
		else if (node->isExternal)
		{
			symbol->firstInitOrder = node->order;
		}

		// TODO: Resolve any type requests within this and underlying scopes
	}

	void visit(AST::SymbolExpression* node) override
	{
		if (node->processed)
			return;
		node->processed = true;

		assert(node->dependency);
		assert(node->dependency->getSymbolSource());

		// Process dependency until we are dependent on a single symbol
		//	since we are possibly dependent on a placeholder source
		while(!node->dependency->getSymbolSource()->isSingleSymbolSource())
		{
			node->dependency->getSymbolSource()->getNode()->accept(this);
		}

		// Make sure that symbol source is processed
		node->dependency->getSymbolSource()->getNode()->accept(this);

		// TODO: It does not work to use firstInitOrder with evals
		// 	since they inject code, currently getting wrongly ordered
		//	Consider doing this at a later pass, in some execution-order
		//	based traversal.
		Symbol* symbol = node->getSymbol();
		if (symbol->firstInitOrder > node->order)
		{	
			// TODO: add line/column
			printLine(string("Warning: Symbol '") + node->symbol + "' is used before initialization" + 
					"(InitOrder: " + std::to_string(symbol->firstInitOrder) + ", RefOrder: " + std::to_string(node->order) + ")");
		}
	}

	void visit(AST::EvalStatement* node) override
	{
		if (node->processed)
			return;
		node->processed = true;	

		//printLine(string("Processing eval: ") + std::to_string(node->order));

		assert(node->expr);
		assert(!node->isGenerated);
		assert(node->catchAllSource);

		node->isGenerated = true;

		Value nodeVal;
		if (!evaluateExpression(node->expr, &nodeVal))
		{
			assert("Cannot evaluate expression" && false);
		}

		auto& type = nodeVal.type;
		const ArrayClass* ac = type.isArray() ? &type.typeClass->as<ArrayClass>() : nullptr;
		if (!ac || !ac->type.isPrimitive() || !ac->type.typeClass->as<PrimitiveClass>().isChar())
		{
			assert("Expression is not of string type" && false);
		}

		assert(node->scopeRef);
		auto* currentScope = node->scopeRef;

		const char* text = nodeVal.data.data();
		const uint length = nodeVal.data.size();

		BufferSourceInput bufferInput(text, length);
		Parser parser(&bufferInput, currentScope);

		AST::Statement* statement;
		while (parser.parseStatement(&statement))
		{
			node->statements.push_back(statement);
		}

		if (parser.getParserErrors().size() != 0 && parser.getScannerErrors().size() != 0)
		{
			// TODO: Print errors etc
			assert("Eval statement contained errors" && false);
		}

		// TODO: Introduce a single node for statement lists
		// First, add all new declarations to the current scope
		for (AST::Statement* s : node->statements)
		{
			processDeclarations(s, currentScope);
		}

		//printLine("Eval statement had these dependencies:", 1);
		//for (auto d : node->catchAllSource->dependencies)
		//	printLine(d->symbolName, 2);

		// Now we can remove the catch-all source that we added earlier
		//	and hook up all the previously caught dependencies
		const bool wasRemoved = currentScope->removeSymbolSource(node->catchAllSource);
		assert(wasRemoved);
		for (auto d : node->catchAllSource->dependencies)
		{
			resolveDepdendency(d, currentScope);
		}

		//printLine(string("Resolving dependencies for ") + std::to_string(node->statements.size()) +
		//	" evaled statements..." , 1);

		// Resolve dependencies in newly created asts
		for (AST::Statement* s : node->statements)
		{
			resolveDependencies(s);
		}

		//printLine(string("Processing ") + std::to_string(node->statements.size()) +
		//	" evaled statements..." , 1);

		// Continue traversal
		for (AST::Statement* s : node->statements)
		{
			s->accept(this);
		}

		//printLine(string("Eval: ") + std::to_string(node->order) + " finished processing");
	}

};

void processAST(AST::Node* root)
{
	processDeclarations(root);
	resolveDependencies(root);

	LOG("Processing ast...");
	{
		ASTProcessor ap;
		root->accept(&ap);
	}

	bool unresolved = false;
	for (SymbolDependency* dep : s_unresolvedDependencies)
	{
		printLine(string("Could not resolve symbol ") + dep->symbolName);
		unresolved = true;
	}
	if (unresolved)
		assert(false && "Had unresolved symbols");
}

void processAST(AST::ASTObject* ast)
{
	processAST(ast->root);
}
