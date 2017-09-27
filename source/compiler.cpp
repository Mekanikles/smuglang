
#include <iostream>
#include <sstream>
#include <regex>
#include <limits>

#include <fstream>
#include <cctype>

#include "core.h"
#include "input.h"
#include "token.h"
#include "scanner.h"
#include "symbols.h"
#include "ast.h"
#include "parser.h"
#include "generator.h"
#include "output.h"

struct SymbolDeclarationScanner : AST::Visitor
{
	SymbolDeclarationScanner()
	{}

	void visit(AST::Module* node) override
	{
		m_currentScope = &node->scope;
		AST::Visitor::visit(node);
	}

	void visit(AST::SymbolDeclaration* node) override
	{
		assert(node->typeExpr);
		
		Symbol* symbol;
		// Make sure symbol is not declared in same scope
		// 	It is okay to overshadow parent scope declarations.
		if (m_currentScope->getSymbolInScope(node->symbol, &symbol))
		{
			assert(false);
		}

		// TODO: Add type information
		symbol = createSymbol(node->symbol);
		symbol->declNode = node;
		m_currentScope->addSymbol(symbol);
		node->symbolObj = symbol;
	}

	SymbolScope* m_currentScope = nullptr;	
};

struct SymbolExpressionScanner : AST::Visitor
{
	SymbolExpressionScanner()
	{}

	void visit(AST::Module* node) override
	{
		m_currentScope = &node->scope;
		AST::Visitor::visit(node);
	}

	Symbol* findUnresolved(const string& symbol)
	{
		for (Symbol* s : m_unresolvedSymbols)
		{
			if (symbol == s->name)
			{
				return s;
			}
		}

		return nullptr;
	}

	void visit(AST::SymbolExpression* node) override
	{
		Symbol* symbol;
		if (!m_currentScope->lookUpSymbolName(node->symbol, &symbol))
		{
			symbol = findUnresolved(node->symbol);
			if (!symbol)
			{
				printLine(string("Modules does not define symbol: ") + node->symbol);
				symbol = createSymbol(node->symbol);
				m_unresolvedSymbols.push_back(symbol);
			}
		}	
		
		node->symbolObj = symbol;
	}

	SymbolScope* m_currentScope = nullptr;
	vector<Symbol*> m_unresolvedSymbols;
};

vector<Symbol*> m_externalSymbols;

void resolveSymbols(AST::AST* ast)
{
	SymbolDeclarationScanner sd;
	ast->root->accept(&sd);

	SymbolExpressionScanner se;
	ast->root->accept(&se);

	for (auto* s : se.m_unresolvedSymbols)
	{
		if (s->name == "int")
		{
			printLine(string("Found primitive symbol: ") + s->name);
			m_externalSymbols.push_back(s);
		}
		else
		{
			printLine(string("Could not resolve symbol: ") + s->name);
			assert(false);
		}
	}
}

int main(int argc, char** argv)
{
	vector<string> args(argv + 1, argv + argc);

	if (args.size() < 1)
		ERROR("No input files specified");

	std::ifstream inFile(args[0]);

	BufferedInputStream inStream(inFile);
	ScannerFactory scannerFactory(inStream);
	AST::AST ast;

	LOG("Parsing...");
	if (!parse(&scannerFactory, &ast))
	{
		LOG("Parse fail!");
		auto f = std::ifstream(args[0]);
		printScannerErrors(f);
		printParserErrors(f);
	}
	else
	{
		LOG("Parse success!");
		printLine("Tokens:");
		printTokens(s_tokens);

		LOG("Resolving symbols...");
		resolveSymbols(&ast);

		printLine("AST:");
		printAST(&ast, 1);

		std::stringstream output;
		CGenerator generator(&output);
		generator.run(&ast);

		std::regex filenameRegex(R"((.*[\\\/])?(.+)$))");
		std::smatch matches;

		assert(std::regex_search(args[0], matches, filenameRegex));
		{
			string outFileName = string(".smug/") + matches[2].str() + ".c";
			std::ofstream outFile(outFileName);

			printLine("Generated C:");
			string l;
			while (getline(output, l))
			{
				printLine(l, 1);
				outFile << l << std::endl;
			}
			std::cout << outFileName;
		}
	}
}

