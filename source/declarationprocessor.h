#pragma once

struct DeclarationProcessor : public AST::Visitor
{
	Context* context;
	SymbolScope* currentScope;

	DeclarationProcessor(Context* context, SymbolScope* initialScope)
		: context(context)
		, currentScope(initialScope)
	{
	}	

	// Hack? Store scope on each ast node, so we can jump between
	//	dependencies when processing
	// TODO: Do this when parsing?
	void visit(AST::Node* node) override
	{
		//printLine(string("Declaration Processor visited node: ") + node->toString(this->context));
		this->context->setScope(node, this->currentScope);
		AST::Visitor::visit(node);
	}

	void visit(AST::StatementBody* node) override
	{
		this->currentScope = context->createScope(this->currentScope);
		AST::Visitor::visit(node);
		this->currentScope = this->currentScope->parentScope;
	}

	void visit(AST::FunctionSignature* node) override
	{
		// Signature has its own scope
		this->currentScope = context->createScope(this->currentScope);

		auto& paramScope = *this->currentScope;

		for (AST::FunctionInParam* param : node->inParams)
		{
			Symbol* symbol = createSymbol(param->name);
			symbol->isParam = true;

			auto symbolSource = createDeclarationSymbolSource(this->context, symbol, param, StorageQualifier::Const);
			paramScope.addSymbolSource(symbolSource);
			this->context->setSymbolSource(param, symbolSource);

			//printLine(string("Created symbol: ") + symbol->name + " in scope: " + std::to_string((long)paramScope.id));		
		}

		for (AST::FunctionOutParam* param : node->outParams)
		{
			Symbol* symbol = createSymbol(param->name);
			symbol->isParam = true;

			auto symbolSource = createDeclarationSymbolSource(this->context, symbol, param, StorageQualifier::Var);
			paramScope.addSymbolSource(symbolSource);
			this->context->setSymbolSource(param, symbolSource);

			//printLine(string("Created symbol: ") + symbol->name + " in scope: " + std::to_string((long)paramScope.id));		
		}

		AST::Visitor::visit(node);

		this->currentScope = this->currentScope->parentScope;
	}

	void visit(AST::FunctionLiteral* node) override
	{
		visit(node->signature);

		auto* signatureScope = this->context->getScope(node->signature);
		assert(signatureScope);

		// Inject signature scope above statement body
		assert(this->currentScope != signatureScope);
		this->currentScope = signatureScope;
		visit(node->body);
		this->currentScope = this->currentScope->parentScope;		
	}

	// TODO: Copy paste code, generalize declarations somehow
	void visit(AST::FunctionDeclaration* node) override
	{
		// Make sure symbol is not declared in same scope
		// 	It is okay to overshadow parent scope declarations.		
		auto existingDeclaration = this->currentScope->lookUpDeclarationInScope(node->symbol);
		if (existingDeclaration)
		{
			assert(false && "Function already declared");
		}

		Symbol* symbol = createSymbol(node->symbol, createFunctionType());
		// InitOrder of functions is 0 since they are initialized at compile time
		symbol->firstInitOrder = 0;

		auto symbolSource = createDeclarationSymbolSource(this->context, symbol, node, StorageQualifier::Def);
		this->currentScope->addSymbolSource(symbolSource);
		this->context->setSymbolSource(node, symbolSource);

		//printLine(string("Created symbol: ") + symbol->name + " in scope: " + std::to_string((long)this->currentScope->id));
		AST::Visitor::visit(node);
	}

	void visit(AST::SymbolDeclaration* node) override
	{
		// Make sure symbol is not declared in same scope
		// 	It is okay to overshadow parent scope declarations.
		auto exitingDeclaration = this->currentScope->lookUpDeclarationInScope(node->symbol);
		if (exitingDeclaration)
		{
			assert(false && "Symbol already declared");
		}

		// TODO: If this symbol is referenced in the type or init expr
		//	Can it lead to an infinite recursion when inferring types?
		Symbol* symbol = createSymbol(node->symbol);

		auto symbolSource = createDeclarationSymbolSource(this->context, symbol, node, node->storageQualifier);
		this->currentScope->addSymbolSource(symbolSource);
		this->context->setSymbolSource(node, symbolSource);

		//printLine(string("Created symbol: ") + symbol->name + " in scope: " + std::to_string((long)this->currentScope->id));
		AST::Visitor::visit(node);
	}

	void visit(AST::EvalStatement* node) override
	{
		// TODO: This might not be relevant if evals have their own scope
		// Eval statments can potentially provide any symbol declaration
		//	so we add a catch-all symbol source and deal with them later
		// 	when we are able to eval the expression
		//auto* catchAllSource = createCatchAllSymbolSource(this->context, node);
		//this->context->setCatchAllSymbolSource(node, catchAllSource);

		// Hack: to avoid circular dependencies by default, we don't want to 
		//	include the expression in the eval scope, again for circular deps.
		AST::Visitor::visit(node->expr);
		auto innerScope = context->createScope(this->currentScope);
		this->context->setScope(node, innerScope);
		//innerScope->addSymbolSource(catchAllSource);
	}
};

void processDeclarations(Context* context, AST::Node* root, SymbolScope* initialScope = nullptr)
{
	//LOG("Processing declarations...");
	DeclarationProcessor dp(context, initialScope);
	root->accept(&dp);
}


