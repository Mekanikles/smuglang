
#include <iostream>
#include <sstream>
#include <regex>
#include <limits>

#include <fstream>
#include <cctype>

#include "core.h"
#include "utils.h"
#include "input.h"
#include "token.h"
#include "scanner.h"
#include "types.h"
#include "symbols.h"
#include "ast.h"
#include "evaluation.h"
#include "parser.h"
#include "output.h"
#include "generator.h"

/*
struct SymbolDeclarationScanner : AST::Visitor
{
	SymbolDeclarationScanner()
	{}

	void visit(AST::StatementBody* node) override
	{
		auto oldScope = m_currentScope;
		m_currentScope = &node->scope;
		AST::Visitor::visit(node);
		m_currentScope = oldScope;
	}

	// TODO: Copy paste code, generalize declarations somehow
	void visit(AST::FunctionDeclaration* node) override
	{
		//assert(node->typeExpr);
		
		Symbol* symbol;
		if (m_currentScope->getSymbolInScope(node->symbol, &symbol))
		{
			assert(false);
		}

		symbol = createSymbol(node->symbol);
		symbol->declNode = node;
		symbol->isFunction = true;
		m_currentScope->addSymbol(symbol);
		node->symbolObj = symbol;

		//printLine(string("Created symbol: ") + symbol->name + " in scope: " + std::to_string((long)m_currentScope));

		AST::Visitor::visit(node);
	}

	void visit(AST::SymbolDeclaration* node) override
	{
		Symbol* symbol;
		// Make sure symbol is not declared in same scope
		// 	It is okay to overshadow parent scope declarations.
		if (m_currentScope->getSymbolInScope(node->symbol, &symbol))
		{
			assert(false);
		}

		symbol = createSymbol(node->symbol);
		symbol->declNode = node;
		symbol->isParam = node->isParam;
		m_currentScope->addSymbol(symbol);
		node->symbolObj = symbol;

		//printLine(string("Created symbol: ") + symbol->name + " in scope: " + std::to_string((long)m_currentScope));

		AST::Visitor::visit(node);
	}

	void visit(AST::FunctionLiteral* node) override
	{
		assert(node->body);
		auto oldScope = m_currentScope;
		m_currentScope = &node->body->scope;
		AST::Visitor::visit(node);
		m_currentScope = oldScope;
	}

	SymbolScope* m_currentScope = nullptr;	
};

struct SymbolExpressionScanner : AST::Visitor
{
	SymbolExpressionScanner()
	{}

	void visit(AST::StatementBody* node) override
	{
		auto oldScope = m_currentScope;
		m_currentScope = &node->scope;
		AST::Visitor::visit(node);
		m_currentScope = oldScope;
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
		Symbol* symbol = nullptr;
		if (node->builtIn)
		{
			if (!lookUpBuiltInSymbolName(node->symbol, &symbol))
			{
				printLine(string("Could not find built in symbol: ") + node->symbol);
				assert(false);
			}
		}
		else
		{
			if (!m_currentScope->lookUpSymbolName(node->symbol, &symbol))
			{
				symbol = findUnresolved(node->symbol);
				if (!symbol)
				{
					printLine(string("Module does not define symbol: ") + node->symbol);
					symbol = createSymbol(node->symbol);
					m_unresolvedSymbols.push_back(symbol);
				}
			}
			else
			{
				if (symbol->declNode->order > node->order)
				{	
					// TODO: add line/column
					// TODO: Replace function check with static, or proper initialization order
					if (!symbol->isFunction)
						printLine(string("Warning: Symbol '") + node->symbol + "' is used before initialization");
				}
			}
		}

		node->symbolObj = symbol;
	}

	SymbolScope* m_currentScope = nullptr;
	vector<Symbol*> m_unresolvedSymbols;
};

vector<Symbol*> m_externalSymbols;


void resolveTypes()
{
	auto symbols = getSymbols(); // TODO: Store symbols per module
	for (Symbol* s : symbols)
	{
		if (s->knowsType())
			continue;

		assert(s->isdeclared());

		Type type;
		if (s->isFunction)
		{
			// TODO: Fix this for external declarations
			AST::FunctionDeclaration* node = (AST::FunctionDeclaration*)s->declNode;
			if (node->funcLiteral)
				type = node->funcLiteral->getType();
			else
				type.isFunction = true;
		}
		else
		{		
			AST::SymbolDeclaration* node = (AST::SymbolDeclaration*)s->declNode;	
			printLine(string("Resolving type for: ") + node->toString());
			// TODO: Add type information
			if (node->typeExpr)
			{
				type = node->typeExpr->getType();
				assert(type.isType);
				// TODO: Ugh, whats going on here, how to describe the value of a type?
				type.isType = false;
				if (node->initExpr)
				{
					Type t2 = node->initExpr->getType();
					assert(isAssignable(type, t2)); 
				}
			}
			else
			{
				assert(node->initExpr);
				type = node->initExpr->getType();
			}
		}

		s->type = type;
	}
}


void resolveSymbols(AST::AST* ast)
{
	createBuiltInSymbols();

	SymbolDeclarationScanner sd;
	ast->root->accept(&sd);

	SymbolExpressionScanner se;
	ast->root->accept(&se);

	// TODO: Make this generic external module lookup
	//	Should external symbol information be "copied" into the module
	//	so that it does not need to redo type resolving on next compile?
	for (auto* s : se.m_unresolvedSymbols)
	{
		if (s->name == "int")
		{
			s->type.isInt = true;
			m_externalSymbols.push_back(s);
		}
		else
		{
			printLine(string("Could not resolve symbol: ") + s->name);
			assert(false);
		}
	}

	resolveTypes();
}
*/

struct SymbolResolver : AST::Visitor
{
	void visit(AST::StatementBody* node) override
	{
		auto oldScope = m_currentScope;
		m_currentScope = &node->scope;
		AST::Visitor::visit(node);
		m_currentScope = oldScope;
	}

	// TODO: Copy paste code, generalize declarations somehow
	void visit(AST::FunctionDeclaration* node) override
	{
		//assert(node->typeExpr);
		
		Symbol* symbol;
		if (m_currentScope->getSymbolInScope(node->symbol, &symbol))
		{
			assert(false);
		}

		symbol = createSymbol(node->symbol, node);
		symbol->type = createFunctionType();
		// TODO: Order of external function decls is probably -1
		symbol->firstInitOrder = node->order;
		m_currentScope->addSymbol(symbol);
		node->symbolObj = symbol;

		//printLine(string("Created symbol: ") + symbol->name + " in scope: " + std::to_string((long)m_currentScope));

		AST::Visitor::visit(node);
	}

	void visit(AST::SymbolDeclaration* node) override
	{
		Symbol* symbol;
		// Make sure symbol is not declared in same scope
		// 	It is okay to overshadow parent scope declarations.
		if (m_currentScope->getSymbolInScope(node->symbol, &symbol))
		{
			assert(false && "Symbol already declared");
		}

		// TODO: If this symbol is referenced in the type or init expr
		//	Can it lead to an infinite recursion when inferring types?
		symbol = createSymbol(node->symbol, node);
		symbol->isParam = node->isParam;
		m_currentScope->addSymbol(symbol);
		node->symbolObj = symbol;

		// Check explicit type
		// TODO: How to assign "type" if inner type is always transferred?
		//	i.e var x : type; "type" needs to be a type variable that
		//	has an inner type of a type variable?
		if (node->typeExpr)
		{
			node->typeExpr->accept(this);
			const Type type = node->typeExpr->getType();
			symbol->type = type.innerTypeFromTypeVariable();
		}

		// Infer type from init expression
		if (node->initExpr)
		{
			symbol->firstInitOrder = node->order;
			node->initExpr->accept(this);
			Type& exprType = node->initExpr->getType();
			const auto result = unifyTypes(symbol->type, exprType);

			// TODO: Handle implicit casts?
			if (result == CannotUnify)
				assert("Cannot unify types" && false);

			// TODO: How to apply unification to expression?
		}

		// TODO: Resolve any type requests within this and underlying scopes


		//printLine(string("Created symbol: ") + symbol->name + " in scope: " + std::to_string((long)m_currentScope));
	}

	void visit(AST::SymbolExpression* node) override
	{
		Symbol* symbol = nullptr;
		if (m_currentScope->lookUpSymbolName(node->symbol, &symbol))
		{
			assert(symbol->declNode);
			if (symbol->firstInitOrder > node->order)
			{	
				// TODO: add line/column
				// TODO: Replace function check with static, or proper initialization order
				if (!symbol->isFunction)
					printLine(string("Warning: Symbol '") + node->symbol + "' is used before initialization");
			}
		}
		else
		{
			node->symbolRequest = createSymbolRequest(node->symbol, node);
			// TODO: When resolving requests, type must be unified again, 
			//	but from the bottom up in the AST. HOW?
		}

		node->symbolObj = symbol;
	}


	SymbolScope* m_currentScope = nullptr;	
};


void resolveSymbols(AST::AST* ast)
{
	SymbolResolver resolver;
	ast->root->accept(&resolver);
}

struct EvalProcessor : AST::Visitor
{
	void visit(AST::EvalStatement* node) override
	{
		assert(node->expr);
		assert(!node->isGenerated);

		node->isGenerated = true;

		Value nodeVal;
		if (!evaluateExpression(node->expr, &nodeVal))
		{
			assert("Cannot evaluate expression" && false);
		}

		auto& type = nodeVal.type;
		const ArrayClass* ac = type.isArray() ? &type.typeClass->as<ArrayClass>() : nullptr;
		if (!ac || !ac->type.isPrimitive() || !ac->type.typeClass->as<PrimitiveClass>().isChar())
		{
			assert("Expression is not of string type" && false);
		}

		const char* text = nodeVal.data.data();
		const uint length = nodeVal.data.size();

		MemoryInputStream file(text, length);
		Parser parser(file);

		AST::Statement* statement;
		while (parser.parseStatement(&statement))
		{
			node->statements.push_back(statement);
		}

		if (parser.getParserErrors().size() != 0 && parser.getScannerErrors().size() != 0)
		{
			// TODO: Print errors etc
			assert("Eval statement contained errors" && false);
		}
	}
};

void processEvals(AST::AST* ast)
{
	EvalProcessor ep;
	ast->root->accept(&ep);
}

void processAST(AST::AST* ast)
{
	LOG("Processing evals...");	
	processEvals(ast);


	LOG("Resolving symbols...");
	resolveSymbols(ast);
}

int main(int argc, char** argv)
{
	vector<string> args(argv + 1, argv + argc);

	if (args.size() < 1)
		ERROR("No input files specified");

	std::ifstream inFile(args[0]);
	AST::AST ast;

	LOG("Parsing...");
	Parser parser(inFile);
	bool parseSuccess = parser.parse(&ast);
	printLine("Tokens:");
	printTokens(parser.getTokens());
	if (!parseSuccess)
	{
		LOG("Parse fail!");
		auto f = std::ifstream(args[0]);
		printScannerErrors(f);
		printParserErrors(f);
	}
	else
	{
		LOG("Parse success!");
		processAST(&ast);

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

