#pragma once

string debugName(SymbolSource* o) 
{
	return string("Symbol Source, node: " +
		o->getNode()->toString(o->getContext()) + ", order: " + std::to_string(o->getNode()->order));
	return "Unknown"; 
}

vector<SymbolDependency*> s_unresolvedDependencies;

SymbolSource* resolveDependency(SymbolDependency* dependency, SymbolScope* scope)
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
		AST::Visitor::visit(node);

		SymbolDependency* dependency = createSymbolDepedency(node->symbol);
		this->context->setSymbolDependency(node, dependency);

		auto scope = this->context->getScope(node);
		assert(scope);
		resolveDependency(dependency, scope);
	}

	void visit(AST::TemplateDeclaration* node) override
	{
		// Do not traverse subtree
	}	

	DependencyResolver(ASTContext* context)
		: context(context)
	{
	}

	ASTContext* context;
};

void resolveDependencies(ASTContext* context, AST::Node* root)
{
	//LOG("Resolving dependencies...");

	DependencyResolver dr(context);
	root->accept(&dr);
}
