
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

vector<Symbol*> s_unresolvedSymbols;

struct SymbolResolver : AST::Visitor
{
	SymbolResolver()
	{}

	void visit(AST::Module* node) override
	{
		m_currentScope = &node->scope;
		AST::Visitor::visit(node);
	}

	void visit(AST::SymbolDeclaration* node) override
	{
		assert(node->typeExpression);
		
		Symbol* symbol;
		// Make sure symbol is not declared in same scope
		// 	It is okay to overshadow parent scope declarations.
		if (m_currentScope->getSymbolInScope(node->symbol, &symbol))
		{
			assert(false);
		}

		// TODO: Add type information
		symbol = createSymbol(node->symbol);
		m_currentScope->addSymbol(symbol);
		node->symbolObj = symbol;
	}

	void visit(AST::SymbolExpression* node) override
	{
		Symbol* symbol;
		if (m_currentScope->lookUpSymbolName(node->symbol, &symbol))
		{
			node->symbolObj = symbol;
		}
		else
		{
			assert(false);
		}
	}

	SymbolScope* m_currentScope = nullptr;
};





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
		SymbolResolver symbolResolver;
		ast.root->accept(&symbolResolver);

		printLine("AST:");
		printAST(&ast, 1);

		std::stringstream output;
		CGenerator generator(&output);
		generator.run(&ast);

		std::regex filenameRegex(R"((.*[\\\/])?(.+)$))");
		std::smatch matches;

		assert(std::regex_search(args[0], matches, filenameRegex));
		{
			string outFileName = string(".smug/") + matches[2].str() + ".smc";
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

