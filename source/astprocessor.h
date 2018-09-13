
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

		Type& functionType = node->expr->getType(this->context);
		assert(functionType.isFunction());
		FunctionClass& function = functionType.getFunction();

		FunctionArgumentBinding* argBinding = createFunctionArgumentBinding(node, function);
		node->argBinding = argBinding;

		unifyArguments(this->context, node, argBinding);

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

		TypeRef& functionType = node->funcLiteral->getType(this->context);

		Symbol* symbol = node->getSymbol(this->context);
		symbol->firstInitOrder = node->order;
		symbol->type = functionType;
	}

	void visit(AST::FunctionSignature* node) override
	{
		// Visit subtree of signature
		AST::Visitor::visit(node);

		// Assign types to inparams
		for (AST::FunctionInParam* param : node->inParams)
		{
			Symbol* symbol = param->getSymbol(this->context);

			TypeRef type;
			if (param->typeExpr)
			{
				const TypeRef& exprType = param->typeExpr->getType(this->context);
				type = exprType->getTypeVariable().type;
			}

			const bool isVariadic = param->isVariadic;
			if (isVariadic)
				symbol->getType() = createTupleType(std::move(type));
			else
				symbol->getType() = type;

			// Infer type from init expression
			if (param->initExpr && !isVariadic)
			{
				symbol->firstInitOrder = node->order;
				if (param->initExpr)
				{
					TypeRef& exprType = param->initExpr->getType(this->context);
					const auto result = unifyTypes(symbol->getType(), exprType);

					// TODO: Handle implicit casts?
					if (result == CannotUnify)
						assert("Cannot unify types" && false);

					// TODO: How to apply unification to expression?
				}
			}
		}

		// Assign types to outparams
		for (AST::FunctionOutParam* param : node->outParams)
		{
			Symbol* symbol = param->getSymbol(this->context);

			TypeRef type;
			if (param->typeExpr)
			{
				const TypeRef& exprType = param->typeExpr->getType(this->context);
				type = exprType->getTypeVariable().type;
			}

			symbol->type = type;
		}

		this->context->addTypeLiteral(node, node->createLiteralType(this->context));
	}

	void visit(AST::StringLiteral* node) override
	{
		this->context->addTypeLiteral(node, node->createLiteralType());
	}

	void visit(AST::IntegerLiteral* node) override
	{
		this->context->addTypeLiteral(node, node->createLiteralType());
	}

	void visit(AST::FloatLiteral* node) override
	{
		this->context->addTypeLiteral(node, node->createLiteralType());
	}

	void visit(AST::TypeLiteral* node) override
	{
		this->context->addTypeLiteral(node, node->createLiteralType());
	}

	void visit(AST::Tuple* node) override
	{
		this->context->addTypeLiteral(node, node->createLiteralType(this->context));
	}

	void visit(AST::UnaryPostfixOp* node) override
	{
		this->context->addTypeLiteral(node, node->createLiteralType(this->context));
	}

	void visit(AST::FunctionLiteral* node) override
	{
		if (node->processed)
			return;
		node->processed = true;

		assert(node->signature);
		assert(node->body);

		node->signature->accept(this);
		node->body->accept(this);
	}

	void visit(AST::SymbolDeclaration* node) override
	{
		if (node->processed)
			return;
		node->processed = true;

		Symbol* symbol = node->getSymbol(this->context);

		// Check explicit type
		// TODO: How to assign "type" if inner type is always transferred?
		//	i.e var x : type; "type" needs to be a type variable that
		//	has an inner type of a type variable?
		if (node->typeExpr)
		{
			node->typeExpr->accept(this);
			const Type& type = node->typeExpr->getType(this->context);
			symbol->type = type.getTypeVariable().type;
		}

		// Infer type from init expression
		if (node->initExpr)
		{
			symbol->firstInitOrder = node->order;
			if (node->initExpr)
			{
				node->initExpr->accept(this);
				TypeRef& exprType = node->initExpr->getType(this->context);
				const auto result = unifyTypes(symbol->getType(), exprType);

				// TODO: Handle implicit casts?
				if (result == CannotUnify)
					assert("Cannot unify types" && false);

				// TODO: How to apply unification to expression?
			}
		}
		else if (node->isExternal())
		{
			symbol->firstInitOrder = node->order;
		}

		// TODO: Resolve any type requests within this and underlying scopes
	}

	void visit(AST::Assignment* node) override
	{
		if (node->processed)
			return;
		node->processed = true;

		assert(node->symExpr);
		node->symExpr->accept(this);
		Symbol* symbol = node->symExpr->getSymbol(this->context);

		if (symbol->firstInitOrder < node->order)
			symbol->firstInitOrder = node->order;

		// Infer type from assignment
		assert(node->expr);
		node->expr->accept(this);

		TypeRef& exprType = node->expr->getType(this->context);
		const auto result = unifyTypes(symbol->type, exprType);

		// TODO: Handle implicit casts?
		if (result == CannotUnify)
			assert("Cannot unify types" && false);	
	}

	void visit(AST::SymbolExpression* node) override
	{
		if (node->processed)
			return;
		node->processed = true;

		auto* dependency = this->context->getSymbolDependency(node);
		assert(dependency);

		// Process dependency until we are dependent on a single symbol
		//	since we are possibly dependent on a placeholder source
		while(!dependency->getSymbolSource()->isSingleSymbolSource())
		{
			dependency->getSymbolSource()->getNode()->accept(this);
		}

		// Make sure that symbol source is processed
		dependency->getSymbolSource()->getNode()->accept(this);

		// TODO: It does not work to use firstInitOrder with evals
		// 	since they inject code, currently getting wrongly ordered
		//	Consider doing this at a later pass, in some execution-order
		//	based traversal.
		Symbol* symbol = node->getSymbol(this->context);
		if (!node->isPartOfAssignment && symbol->firstInitOrder > node->order)
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

		// Make sure dependency is processed
		assert(node->expr);
		node->expr->accept(this);

		assert(!node->isGenerated);

		node->isGenerated = true;

		// TODO: This happens too early somehow, at this point we might have a multitype
		//	string for example. We need to concretizie the type here, or at least handle
		//	all possible types of strings. Either way we need to know the type first before 
		//	we can evaluate the data (different data depending on zstrings/arrays etc)
		Value nodeVal;
		if (!evaluateExpression(this->context, node->expr, &nodeVal))
		{
			assert("Cannot evaluate expression" && false);
		}

		auto& type = nodeVal.type;
		const ArrayClass* ac = type->isArray() ? &type->typeClass->as<ArrayClass>() : nullptr;
		if (!ac || !ac->type->isPrimitive() || !ac->type->typeClass->as<PrimitiveClass>().isChar())
		{
			assert("Expression is not of string type" && false);
		}

		const char* text = nodeVal.data.data();
		const uint length = nodeVal.data.size();

		BufferSourceInput bufferInput(text, length);
		Parser parser(&bufferInput);

		AST::Statement* statement;
		while (parser.parseStatement(&statement))
		{
			node->statements.push_back(statement);
		}

		auto* currentScope = this->context->getScope(node);
		assert(currentScope);
		
		if (parser.getParserErrors().size() != 0 && parser.getScannerErrors().size() != 0)
		{
			// TODO: Print errors etc
			assert("Eval statement contained errors" && false);
		}

		// TODO: Introduce a single node for statement lists
		// First, add all new declarations to the current scope
		for (AST::Statement* s : node->statements)
		{
			processDeclarations(this->context, s, currentScope);
		}

		//printLine("Eval statement had these dependencies:", 1);
		//for (auto d : node->catchAllSource->dependencies)
		//	printLine(d->symbolName, 2);

		auto* catchAllSource = this->context->getCatchAllSymbolSource(node);
		assert(catchAllSource);

		// Now we can remove the catch-all source that we added earlier
		//	and hook up all the previously caught dependencies
		const bool wasRemoved = currentScope->removeSymbolSource(catchAllSource);
		assert(wasRemoved);

		for (auto d : catchAllSource->dependencies)
		{
			resolveDependency(d, currentScope);
		}

		//printLine(string("Resolving dependencies for ") + std::to_string(node->statements.size()) +
		//	" evaled statements..." , 1);

		// Resolve dependencies in newly created asts
		for (AST::Statement* s : node->statements)
		{
			resolveDependencies(this->context, s);
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

	ASTProcessor(Context* context)
		: context(context)
	{
	}

	Context* context;
};

void processAST(Context* context, AST::Node* root)
{
	processDeclarations(context, root);
	resolveDependencies(context, root);

	LOG("Processing ast...");
	{
		ASTProcessor ap(context);
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

void processAST(Context* context, AST::ASTObject* ast)
{
	processAST(context, ast->root);
}
