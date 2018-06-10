
string debugName(SymbolSource* o) 
{
	return string("Symbol Source, node: " +
		o->getNode()->toString() + ", order: " + std::to_string(o->getNode()->order));
	return "Unknown"; 
}

vector<SymbolDependency*> s_unresolvedDependencies;

SymbolSource* resolveDepdendency(SymbolDependency* dependency, SymbolScope* scope)
{
	SymbolSource* symbolSource = nullptr;
	if ((symbolSource = scope->lookUpSymbolSource(dependency->symbolName)))
	{
		//printLine(string("Hooked dependency: ") + dependency->symbolName + 
		//		" in scope: " + std::to_string(scope->id) + " onto: " + debugName(symbolSource));
		symbolSource->hookDependency(dependency);
	} 
	else
	{
		printLine(string("Could not resolve dependency: ") + dependency->symbolName +
				" in scope: " + std::to_string(scope->id) );

		// If we did not find a symbol source, we are either depending on an external
		//	symbol, or a symbol that does not exist
		// Save it for later
		// TODO: Can we leave dependency unhooked? Maybe hook on a root source?
		s_unresolvedDependencies.push_back(dependency);
	}

	return symbolSource;
}

struct DependencyResolver : AST::Visitor
{
	void visit(AST::SymbolExpression* node) override
	{
		SymbolDependency* dependency = createSymbolDepedency(node->symbol);
		node->dependency = dependency;

		assert(node->scopeRef);
		resolveDepdendency(dependency, node->scopeRef);
	}
};

void resolveDependencies(AST::Node* root)
{
	//LOG("Resolving dependencies...");

	DependencyResolver dr;
	root->accept(&dr);
}
