#pragma once

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

		for (auto arg : node->args)
		{
			arg->accept(this);
		}

		Type& functionType = node->expr->getType(this->context);
		assert(functionType.isFunction());
		FunctionClass& function = functionType.getFunction();

		FunctionArgumentBinding* argBinding = createFunctionArgumentBinding(node, function);
		node->argBinding = argBinding;

		// Here we differentiate between generic and concrete functions
		if (function.isConcrete())
		{
			unifyArguments(this->context, node, argBinding);
		}
		else
		{
			unifyArguments(this->context, node, argBinding);
		}

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

		// Store function for evaluation
		{
			auto source = context->getSymbolSource(node);
			assert(source);
			Evaluation::storeConstantFromExpression(econtext, *context, *node->funcLiteral, *source);	
		}
	}

	void visit(AST::FunctionSignature* node) override
	{
		if (node->processed)
			return;
		node->processed = true;

		// Visit subtree of signature
		AST::Visitor::visit(node);

		// Assign types to inparams
		for (AST::FunctionInParam* param : node->inParams)
		{
			Symbol* symbol = param->getSymbol(this->context);

			TypeRef type;
			if (param->typeExpr)
			{
				Value nodeVal;
				if (!evaluateExpression(this->context, param->typeExpr, &nodeVal))
				{
					assert("Cannot evaluate type expression for in-parameter" && false);
				}
				
				type = nodeVal.type->getTypeVariable().type;
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
				Value nodeVal;
				if (!evaluateExpression(this->context, param->typeExpr, &nodeVal))
				{
					assert("Cannot evaluate type expression for out-parameter" && false);
				}
				
				type = nodeVal.type->getTypeVariable().type;
			}

			symbol->type = type;
		}

		this->context->addTypeLiteral(node, node->createLiteralType(this->context));
	}

	void visit(AST::StringLiteral* node) override
	{
		if (node->processed)
			return;
		node->processed = true;

		this->context->addTypeLiteral(node, node->createLiteralType());
	}

	void visit(AST::IntegerLiteral* node) override
	{
		if (node->processed)
			return;
		node->processed = true;

		this->context->addTypeLiteral(node, node->createLiteralType());
	}

	void visit(AST::FloatLiteral* node) override
	{
		if (node->processed)
			return;
		node->processed = true;

		this->context->addTypeLiteral(node, node->createLiteralType());
	}

	void visit(AST::TypeLiteral* node) override
	{
		if (node->processed)
			return;
		node->processed = true;

		this->context->addTypeLiteral(node, node->createLiteralType());
	}

	void visit(AST::Tuple* node) override
	{
		if (node->processed)
			return;
		node->processed = true;

		vector<TypeRef> types;
		for (auto* e : node->exprs)
		{
			e->accept(this);

			types.push_back(TypeRef(e->getType(context)));
		}

		this->context->addTypeLiteral(node, createTupleType(std::move(types)));
	}

	void visit(AST::UnaryPostfixOp* node) override
	{
		if (node->processed)
			return;
		node->processed = true;
		
		assert(node->expr);
		node->expr->accept(this);

		Value nodeVal;
		if (!evaluateExpression(this->context, node->expr, &nodeVal))
		{
			assert("Cannot evaluate type expression for post fix operator" && false);
		}

		// TODO: Bubble up types through ops for now
		const TypeRef& t = nodeVal.type;
		TypeRef type;
		if (node->opType == TokenType::Asterisk)
		{
			type = createPointerTypeVariable(TypeRef(t.getType().getTypeVariable().type));
		}
		else
		{
			type = t;
		}

		this->context->addTypeLiteral(node, std::move(type));
	}

	void visit(AST::FunctionLiteral* node) override
	{
		if (node->processed)
			return;
		node->processed = true;

		assert(node->signature);
		node->signature->accept(this);

		if (node->signature->outParams.empty())
		{
			m_expectedReturnType = std::optional<TypeRef>();
		}
		else
		{
			assert(node->signature->outParams.size() == 1);
			auto* pnode = node->signature->outParams[0];
			Symbol* symbol = pnode->getSymbol(this->context);
			m_expectedReturnType = symbol->type;
		}
		
		assert(node->body);		
		node->body->accept(this);
	}

	void visit(AST::ReturnStatement* node) override
	{
		if (node->processed)
			return;
		node->processed = true;

		// TODO: This is super similiar to an assignment, maybe convert
		if (node->expr)
		{
			assert(m_expectedReturnType && "Return expression in void function");

			node->expr->accept(this);
			TypeRef& exprType = node->expr->getType(this->context);
			const auto result = unifyTypes(*m_expectedReturnType, exprType);

			// TODO: Handle implicit casts?
			if (result == CannotUnify)
				assert("Cannot unify types for return statement" && false);
		}
		else
		{
			// TODO: Is this true?
			assert(!m_expectedReturnType && "Missing return expression in non-void function");
		}
	}

	void visit(AST::SymbolDeclaration* node) override
	{
		if (node->processed)
			return;
		node->processed = true;

		Symbol* symbol = node->getSymbol(this->context);

		// Check explicit type

		if (node->typeExpr)
		{
			node->typeExpr->accept(this);
			Value nodeVal;
			if (!evaluateExpression(this->context, node->typeExpr, &nodeVal))
			{
				assert("Cannot evaluate type expression for declaration" && false);
			}

			symbol->type = nodeVal.type->getTypeVariable().type;
		}

		// Infer type from init expression
		if (node->initExpr)
		{
			symbol->firstInitOrder = node->order;
			if (node->initExpr)
			{
				node->initExpr->accept(this);
				TypeRef& initExprType = node->initExpr->getType(this->context);
				const auto result = unifyTypes(symbol->getType(), initExprType);

				// TODO: Handle implicit casts?
				if (result == CannotUnify)
					assert("Cannot unify types for declaration" && false);

				// TODO: How to apply unification to expression?
			}
		}
		else if (node->isExternal())
		{
			symbol->firstInitOrder = node->order;

			if (symbol->type->isFunction())
			{
				// Allow some non-concrete functions to be converted to variadics
				if (symbol->type->isFunction())
					symbol->type->getFunction().convertToVariadicIfPossible();
			}

			assert(symbol->type->isConcrete() && "External types need to be explicitly concrete");
		}

		// Evaluate defines and store as constant in IR env
		// 	This allows dependants to look them up
		if (node->isDefine())
		{
			// TODO: Hm, we probably want to store a value for types as well, a hash maybe?
			if (!symbol->getType()->isTypeVariable())
			{
				auto source = context->getSymbolSource(node);
				assert(source);
				assert(node->initExpr);
				Evaluation::storeConstantFromExpression(econtext, *context, *node->initExpr, *source);	
			}
		}  
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
		auto depSource = dependency->getSymbolSource();
		while(!depSource->isSingleSymbolSource())
		{
			depSource->getNode()->accept(this);
			auto newDepSource = dependency->getSymbolSource();
			assert(depSource != newDepSource && "Could not resolve catch-all dependency! (probably a circular dependency on eval)");
			depSource = newDepSource;
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
			assert("Cannot evaluate expression for eval statement" && false);
		}

		if (!isStringType(nodeVal.type))
		{
			assert("Expression is not of string type" && false);
		}

		// TODO: handle other strings
		assert(nodeVal.type.getType().isPointer());
		auto& type = nodeVal.type.getType().getPointer().type;
		assert(type->isPrimitive());
		assert(type->getPrimitive().isChar());
		assert(type->getSize() == 8);

		const char** text = (const char**)nodeVal.data.data();
		const uint length = strnlen(*text, 4096);
		
		// TODO: Compare with previous nodeVal
		string str = Evaluation::evaluateExpressionAsString(econtext, *context, *node->expr);
		assert(!str.compare(*text));

		printLine("Parsing eval string: "); 
		printLine(*text, 1);
 
		BufferSourceInput bufferInput(*text, length);
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
			printLine("Eval statement contained errors:");
			printScannerErrors(parser);	
			printParserErrors(parser);
			// TODO: Handle eval errors
			assert(false);
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

		/*auto* catchAllSource = this->context->getCatchAllSymbolSource(node);
		assert(catchAllSource);

		// Now we can remove the catch-all source that we added earlier
		//	and hook up all the previously caught dependencies
		const bool wasRemoved = currentScope->removeSymbolSource(catchAllSource);
		assert(wasRemoved);

		for (auto d : catchAllSource->dependencies)
		{
			resolveDependency(d, currentScope);
		}*/

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

	ASTProcessor(EvaluationContext& econtext, ASTContext* context)
		: econtext(econtext)
		, context(context)
	{
	}

	std::optional<TypeRef> m_expectedReturnType;
	EvaluationContext& econtext;
	ASTContext* context;
};

void processAST(EvaluationContext& econtext, ASTContext* context, AST::Node* root)
{
	processDeclarations(context, root);
	resolveDependencies(context, root);

	LOG("Processing ast...");
	{
		ASTProcessor ap(econtext, context);
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

void processAST(EvaluationContext& econtext, ASTContext* context, AST::ASTObject* ast)
{
	processAST(econtext, context, ast->module);
}
