#pragma once

#include <vector>

namespace AST
{
	struct SymbolDeclaration;
	struct Declaration;
}

struct Symbol
{
	string name;
	// TODO Type;

	Type type;
	bool isParam = false;
	bool isFunction = false;

	AST::Declaration* declNode = nullptr;

	bool isdeclared()
	{
		return declNode != nullptr;
	}

	bool knowsType()
	{
		return type.isInt == true ||
			   type.isFunction == true ||
			   type.isFloat == true ||
			   type.isString == true;
	}
};

vector<Symbol*> s_symbols;

Symbol* createSymbol(string name)
{
	Symbol* s = new Symbol { name };
	s_symbols.push_back(s);
	return s;
}

vector<Symbol*>& getSymbols()
{
	return s_symbols;
}

struct SymbolScope
{
	SymbolScope* parentScope = nullptr;

	vector<Symbol*> symbols;

	void addSymbol(Symbol* symbol)
	{
		symbols.push_back(symbol);
	}

	bool getSymbolInScope(const string& name, Symbol** outSymbol)
	{
		for (auto* s : symbols)
		{
			if (s->name == name)
			{
				*outSymbol = s;
				return true;
			}
		}

		return false;
	}

	bool lookUpSymbolName(const string& name, Symbol** outSymbol)
	{
		if (parentScope)
		{
			if (parentScope->lookUpSymbolName(name, outSymbol))
				return true;
		}

		return getSymbolInScope(name, outSymbol);
	}
};








