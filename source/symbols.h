#pragma once

#include <vector>
#include <algorithm>

namespace AST
{
	struct Node;
	struct SymbolDeclaration;
	struct Declaration;
	struct SymbolExpression;
	struct TemplateDeclaration;
}

struct ASTContext;

enum class StorageQualifier
{
	Var,
	Const,
	Def,
	Extern
};

string sqToString(StorageQualifier s)
{
	switch(s)
	{
		case StorageQualifier::Var: return "Var";
		case StorageQualifier::Const: return "Const";
		case StorageQualifier::Def: return "Def";
		case StorageQualifier::Extern: return "Extern";
	}
	return "Unknown";
}

struct Symbol
{
	string name;
	// TODO Type;

	TypeRef type;
	bool isParam = false;
	bool isFunction = false;

	// TODO: Remove node, SymbolSource should contain extra data instead
	//AST::Declaration* declNode = nullptr;
	uint firstInitOrder = ~0U;

	//bool isdeclared()
	//{
	//	return declNode != nullptr;
	//}

	TypeRef& getType() { return type; }
	const TypeRef& getType() const { return type; }
};

vector<Symbol*> s_symbols;

Symbol* createSymbol(string name, TypeRef&& type)
{
	Symbol* s = new Symbol { name, std::move(type) };
	s_symbols.push_back(s);
	//s->declNode = declNode;
	return s;
}

Symbol* createSymbol(string name)
{
	return createSymbol(name, Type());
}

vector<Symbol*>& getSymbols()
{
	return s_symbols;
}

struct SymbolScope;
struct TemplateSymbolSource;
struct SymbolDependency;

struct SymbolSource
{
	ASTContext* context;

	// TODO: This should not be necessary, SymbolScope knows what to do and
	//	should store sources by name
	virtual bool providesSymbolName(const string& s) = 0;
	virtual void hookDependency(SymbolDependency* dependency) = 0;
	virtual bool isSingleSymbolSource() = 0;
	virtual bool isDefine() { return false; }
	virtual Symbol* getSymbol() const = 0;
	virtual pair<AST::Node*, ASTContext*> getNode() = 0;
	virtual bool isExternal() { return false; }
	virtual bool isTemplate() { return false; }
	virtual TemplateSymbolSource* asTemplate() { assert(false); return nullptr; }
};

struct SingleSymbolSource : SymbolSource
{
	StorageQualifier storageQualifier;

	bool isSingleSymbolSource() override { return true; }
	bool isDefine() override { return storageQualifier == StorageQualifier::Def; } 
};

struct DeclarationSymbolSource : SingleSymbolSource
{
	// TODO: This should be AST::Declaration but functions Params are not
	//	statements
	AST::Node* node;
	Symbol* symbol = nullptr;

	bool providesSymbolName(const string& s) override { return s == symbol->name; }
	void hookDependency(SymbolDependency* dependency) override;
	Symbol* getSymbol() const override { assert(symbol); return symbol; }
	pair<AST::Node*, ASTContext*> getNode() override { assert(node && context); return pair<AST::Node*, ASTContext*> { node, context }; }
	bool isExternal() override { return storageQualifier == StorageQualifier::Extern; }
};

struct TemplateSymbolSource : SingleSymbolSource
{
	// TODO: This should be AST::Declaration but requires casting
	//	to AST::Node
	AST::Node* node;
	string symbolName;

	bool providesSymbolName(const string& s) override { return s == symbolName; }
	void hookDependency(SymbolDependency* dependency) override;
	Symbol* getSymbol() const override { assert(false && "Cannot resolve a single symbol from template source without parameters"); return nullptr; }
	pair<AST::Node*, ASTContext*> getNode() override { assert(node && context); return pair<AST::Node*, ASTContext*> { node, context }; }
	bool isTemplate() override { return true; }	
	virtual TemplateSymbolSource* asTemplate() override { return this; }
};

struct CatchAllSymbolSource : SymbolSource
{
	AST::Node* node;
	vector<SymbolDependency*> dependencies;

	// Catch all symbol requests
	bool providesSymbolName(const string& s) override { return true; }
	void hookDependency(SymbolDependency* dependency) override;
	bool isSingleSymbolSource() override { return false; }
	Symbol* getSymbol() const override { assert(false && "Cannot resolve a single symbol from catch-all source"); return nullptr; }
	pair<AST::Node*, ASTContext*> getNode() override { assert(node && context); return pair<AST::Node*, ASTContext*> { node, context }; }
};

struct SymbolDependency
{
	SymbolSource* source = nullptr;
	string symbolName;

	SymbolSource* getHookedSymbolSource()
	{
		assert(source);
		return source;
	}

	Symbol* getSymbol() const
	{
		assert(source);
		return source->getSymbol();
	}
};

void DeclarationSymbolSource::hookDependency(SymbolDependency* dependency)
{
	dependency->source = this;
}

void TemplateSymbolSource::hookDependency(SymbolDependency* dependency)
{
	dependency->source = this;
}

void CatchAllSymbolSource::hookDependency(SymbolDependency* dependency)
{
	// Store all symbol requests for later
	dependency->source = this;
	this->dependencies.push_back(dependency);
};

vector<SymbolSource*> s_symbolSources;

DeclarationSymbolSource* createDeclarationSymbolSource(ASTContext* context, Symbol* symbol, AST::Node* node, StorageQualifier storageQualifier)
{
	auto s = new DeclarationSymbolSource();
	s->node = node;
	s->symbol = symbol;
	s->context = context;
	s->storageQualifier = storageQualifier;
	s_symbolSources.push_back(s);
	return s;
}

TemplateSymbolSource* createTemplateSymbolSource(ASTContext* context, string symbolName, AST::Node* node)
{
	auto s = new TemplateSymbolSource();
	s->node = node;
	s->symbolName = symbolName;
	s->context = context;
	s_symbolSources.push_back(s);
	return s;
}

CatchAllSymbolSource* createCatchAllSymbolSource(ASTContext* context, AST::Node* node)
{
	auto s = new CatchAllSymbolSource();
	s->node = node;
	s->context = context;
	s_symbolSources.push_back(s);
	return s;
}

SymbolDependency* createSymbolDepedency(string name)
{
	auto s = new SymbolDependency { nullptr, name };
	return s;
}

static uint s_scopeCount = 0;

struct SymbolScope
{
	uint id = s_scopeCount++;
	SymbolScope* parentScope = nullptr;
	vector<SingleSymbolSource*> singleSymbolSources;
	vector<CatchAllSymbolSource*> catchAllSymbolSources;

	void addSymbolSource(SingleSymbolSource* symbolSource)
	{
		this->singleSymbolSources.push_back(symbolSource);
	}

	void addSymbolSource(CatchAllSymbolSource* symbolSource)
	{
		this->catchAllSymbolSources.push_back(symbolSource);
	}

	bool removeSymbolSource(CatchAllSymbolSource* symbolSource)
	{
		auto it = std::find(this->catchAllSymbolSources.begin(), this->catchAllSymbolSources.end(), symbolSource);
		if (it != this->catchAllSymbolSources.end())
		{
			this->catchAllSymbolSources.erase(it);
			return true;
		}
		return false;
	}

	vector<SingleSymbolSource*>& getDeclarations() 
	{
		return this->singleSymbolSources;
	}

	const vector<SingleSymbolSource*>& getDeclarations() const
	{
		return this->singleSymbolSources;
	}

	bool hasCatchAlls() const { return catchAllSymbolSources.size() > 0; }

	SingleSymbolSource* lookUpDeclarationInScope(const string& symbolName)
	{
		for (auto* s : this->singleSymbolSources)
		{
			if (s->providesSymbolName(symbolName))
				return s;
		}

		return nullptr;
	}

	SymbolSource* lookUpSymbolSource(const string& symbolName)
	{
		//printLine(string("Looking for symbol: ") + name + " in scope: " + std::to_string((long)this));

		// Look for declarations first
		if (SymbolSource* s = lookUpDeclarationInScope(symbolName))
			return s;

		// Return any catch-all is there are available
		if (this->catchAllSymbolSources.size() > 0)
			return catchAllSymbolSources[0];

		if (parentScope)
			return this->parentScope->lookUpSymbolSource(symbolName);

		return nullptr;
	}
};








