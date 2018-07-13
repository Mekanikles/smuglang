#pragma once

#include <vector>
#include <algorithm>

namespace AST
{
	struct Node;
	struct SymbolDeclaration;
	struct Declaration;
	struct SymbolExpression;
}

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

struct SymbolDependency;

struct SymbolSource
{
	// TODO: This should not be necessary, SymbolScope knows what to do and
	//	should store sources by name
	virtual bool providesSymbolName(const string& s) = 0;
	virtual void hookDependency(SymbolDependency* dependency) = 0;
	virtual bool isSingleSymbolSource() = 0;
	virtual Symbol* getSymbol() = 0;
	virtual AST::Node* getNode() = 0;
	virtual bool isExternal() { return false; }
};

struct DeclarationSymbolSource : SymbolSource
{
	AST::Node* node;
	Symbol* symbol = nullptr;
	StorageQualifier storageQualifier;

	bool providesSymbolName(const string& s) override { return s == symbol->name; }
	void hookDependency(SymbolDependency* dependency) override;
	bool isSingleSymbolSource() override { return true; }
	Symbol* getSymbol() override { assert(symbol); return symbol; }
	AST::Node* getNode() override { assert(node); return node; }
	bool isExternal() override { return storageQualifier == StorageQualifier::Extern; }
};

struct CatchAllSymbolSource : SymbolSource
{
	AST::Node* node;	
	vector<SymbolDependency*> dependencies;

	// Catch all symbol requgests
	bool providesSymbolName(const string& s) override { return true; }
	void hookDependency(SymbolDependency* dependency) override;
	bool isSingleSymbolSource() override { return false; }
	Symbol* getSymbol() override { assert(false && "Cannot resolve a single symbol from catch-all source"); return nullptr; }
	AST::Node* getNode() override { assert(node); return node; }
};

struct SymbolDependency
{
	SymbolSource* source = nullptr;
	string symbolName;

	SymbolSource* getSymbolSource()
	{
		assert(source);
		return source;
	}

	Symbol* getSymbol()
	{
		assert(source);
		return source->getSymbol();
	}
};

void DeclarationSymbolSource::hookDependency(SymbolDependency* dependency)
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

DeclarationSymbolSource* createDeclarationSymbolSource(Symbol* symbol, AST::Node* node, StorageQualifier storageQualifier)
{
	auto s = new DeclarationSymbolSource();
	s->node = node;
	s->symbol = symbol;
	s->storageQualifier = storageQualifier;
	s_symbolSources.push_back(s);
	return s;
}

CatchAllSymbolSource* createCatchAllSymbolSource(AST::Node* node)
{
	auto s = new CatchAllSymbolSource();
	s->node = node;
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
	vector<DeclarationSymbolSource*> declarationSymbolSources;
	vector<CatchAllSymbolSource*> catchAllSymbolSources;

	void addSymbolSource(DeclarationSymbolSource* symbolSource)
	{
		this->declarationSymbolSources.push_back(symbolSource);
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

	vector<DeclarationSymbolSource*>& getDeclarations() 
	{
		return declarationSymbolSources;
	}

	bool hasCatchAlls() const { return catchAllSymbolSources.size() > 0; }

	DeclarationSymbolSource* lookUpDeclarationInScope(const string& symbolName)
	{
		for (auto* s : this->declarationSymbolSources)
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








