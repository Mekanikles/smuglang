
struct ScopeTrackingVisitor : AST::Visitor
{
	ScopeTrackingVisitor(SymbolScope* initalScope)
		: currentScope(initalScope)
	{}	

	virtual void visit(AST::StatementBody* node) override
	{
		auto oldScope = this->currentScope;
		this->currentScope = &node->scope;
		AST::Visitor::visit(node);
		this->currentScope = oldScope;
	}

	SymbolScope* currentScope;	
};

struct DeclarationProcessor : ScopeTrackingVisitor
{
	using ScopeTrackingVisitor::ScopeTrackingVisitor;
	using ScopeTrackingVisitor::visit;

	// Hack? Store scope on each ast node, so we can jump between
	//	dependencies when processing
	// TODO: Do this when parsing?
	void visit(AST::Node* node) override
	{
		//printLine(string("Declaration Processor visited node: ") + node->toString());
		node->scopeRef = currentScope;
		AST::Visitor::visit(node);
	}

	void visit(AST::FunctionLiteral* node) override
	{
		assert(node->signature);
		AST::FunctionSignature* signature = node->signature;

		assert(node->body);
		auto& paramScope = node->body->scope;

		// Parse signature params here where we have knowledge about the body
		for (AST::FunctionInParam* param : signature->inParams)
		{
			Symbol* symbol = createSymbol(param->name);
			symbol->isParam = true;

			auto symbolSource = createDeclarationSymbolSource(symbol, param, false);
			paramScope.addSymbolSource(symbolSource);
			//node->symbolObj = symbol;

			//printLine(string("Created symbol: ") + symbol->name + " in scope: " + std::to_string((long)paramScope.id));		
		}

		AST::Visitor::visit(node);
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

		Symbol* symbol = createSymbol(node->symbol);
		symbol->type = createFunctionType();
		// InitOrder of functions is 0 since they are initialized at compile time
		symbol->firstInitOrder = 0;

		auto symbolSource = createDeclarationSymbolSource(symbol, node);
		this->currentScope->addSymbolSource(symbolSource);
		node->symbolSource = symbolSource;

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

		auto symbolSource = createDeclarationSymbolSource(symbol, node, node->isExternal);
		this->currentScope->addSymbolSource(symbolSource);
		node->symbolObj = symbol;

		//printLine(string("Created symbol: ") + symbol->name + " in scope: " + std::to_string((long)this->currentScope->id));
		AST::Visitor::visit(node);
	}

	void visit(AST::EvalStatement* node) override
	{
		// Eval statments can potentially provide any symbol declaration
		//	so we add a catch-all symbol source and deal with them later
		// 	when we are able to eval the expression
		auto* catchAllSource = createCatchAllSymbolSource(node);
		this->currentScope->addSymbolSource(catchAllSource);
		assert(!node->catchAllSource);
		node->catchAllSource = catchAllSource;

		AST::Visitor::visit(node);
	}
};

void processDeclarations(AST::Node* root, SymbolScope* initalScope = nullptr)
{
	//LOG("Processing declarations...");
	DeclarationProcessor dp(initalScope);
	root->accept(&dp);
}


