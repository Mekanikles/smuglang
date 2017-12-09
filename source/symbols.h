#pragma once

#include <vector>

namespace AST
{
	struct SymbolDeclaration;
	struct Declaration;
	struct SymbolExpression;
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
};

/*
vector<Symbol*> s_builtinSymbols;

Symbol* createBuiltInSymbol(string name)
{
	Symbol* s = new Symbol { name };
	s_builtinSymbols.push_back(s);
	return s;
}

void createBuiltInSymbols()
{
	auto s = createBuiltInSymbol("s32");
	s->type.type = PrimitiveType::s32;
}

bool lookUpBuiltInSymbolName(const string& name, Symbol** outSymbol)
{
	for (auto* s : s_builtinSymbols)
	{
		if (s->name == name)
		{
			*outSymbol = s;
			return true;
		}
	}

	return false;
}
*/

vector<Symbol*> s_symbols;

Symbol* createSymbol(string name, AST::Declaration* declNode)
{
	Symbol* s = new Symbol { name };
	s_symbols.push_back(s);
	s->declNode = declNode;
	return s;
}

vector<Symbol*>& getSymbols()
{
	return s_symbols;
}


struct SymbolRequest
{
	string name;
	Type type;
	AST::SymbolExpression* exprNode = nullptr;
};

vector<SymbolRequest*> s_symbolRequests;

SymbolRequest* createSymbolRequest(string name, AST::SymbolExpression* exprNode)
{
	auto s = new SymbolRequest { name , Type(), exprNode };
	s_symbolRequests.push_back(s);
	s->exprNode = exprNode;
	return s;
}

vector<SymbolRequest*>& getSymbolRequests()
{
	return s_symbolRequests;
}

struct SymbolScope
{
	SymbolScope* parentScope = nullptr;

	vector<Symbol*> symbols;
	vector<SymbolRequest*> symbolRequests;

	void addSymbol(Symbol* symbol)
	{
		symbols.push_back(symbol);
	}

	void addSymbolRequest(SymbolRequest* symbolRequest)
	{
		symbolRequests.push_back(symbolRequest);
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
		//printLine(string("Looking for symbol: ") + name + " in scope: " + std::to_string((long)this));

		if (getSymbolInScope(name, outSymbol))
			return true;

		//printLine(string("Symbol not found, looking at parent: ") + std::to_string((long)parentScope), 1);

		if (parentScope)
		{
			if (parentScope->lookUpSymbolName(name, outSymbol))
				return true;
		}

		return false;
	}
};








