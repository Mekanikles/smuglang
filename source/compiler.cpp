
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

const uint DEFAULT_INT_SIZE = 32;
const bool DEFAULT_INT_ISSIGNED = true;

#include "types.h"
#include "symbols.h"
#include "ast.h"
#include "evaluation.h"
#include "parser.h"
#include "output.h"
#include "generator.h"
#include "llvmgenerator.h"

string debugName(SymbolSource* o) 
{
	return string("Symbol Source, node: " +
		o->getNode()->toString() + ", order: " + std::to_string(o->getNode()->order));
	return "Unknown"; 
}

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

	// TODO: Copy paste code, generalize declarations somehow
	void visit(AST::FunctionDeclaration* node) override
	{
		// Make sure symbol is not declared in same scope
		// 	It is okay to overshadow parent scope declarations.		
		auto exitingDeclaration = this->currentScope->lookUpDeclarationInScope(node->symbol);
		if (exitingDeclaration)
		{
			assert(false && "Symbol already declared");
		}

		Symbol* symbol = createSymbol(node->symbol, node);
		symbol->type = createFunctionType();
		// InitOrder of functions is 0 since they are initialized at compile time
		symbol->firstInitOrder = 0;

		auto symbolSource = createDeclarationSymbolSource(symbol, node);
		this->currentScope->addSymbolSource(symbolSource);
		node->symbolSource = symbolSource;

		//printLine(string("Created symbol: ") + symbol->name + " in scope: " + std::to_string((long)m_currentScope));

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
		Symbol* symbol = createSymbol(node->symbol, node);
		symbol->isParam = node->isParam;

		auto symbolSource = createDeclarationSymbolSource(symbol, node, node->isExternal);
		this->currentScope->addSymbolSource(symbolSource);
		node->symbolObj = symbol;

		//printLine(string("Created symbol: ") + symbol->name + " in scope: " + std::to_string((long)m_currentScope));
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

void processAST(AST::Node* root);

void processDeclarations(AST::Node* root, SymbolScope* initalScope = nullptr)
{
	//LOG("Processing declarations...");
	DeclarationProcessor dp(initalScope);
	root->accept(&dp);
}

void resolveDependencies(AST::Node* root)
{
	//LOG("Resolving dependencies...");

	DependencyResolver dr;
	root->accept(&dr);
}

struct ASTProcessor : AST::Visitor
{
	void visit(AST::Call* node) override
	{
		if (node->processed)
			return;
		node->processed = true;

		// Make sure function expression is processed
		node->expr->accept(this);

		Type& functionType = node->expr->getType();
		assert(functionType.isFunction());
		FunctionClass& function = functionType.getFunction();
		auto& inTypes = function.inTypes;
		auto& args = node->args;

		if (function.isVariadic)
			assert(args.size() >= inTypes.size());
		else
			assert(inTypes.size() == args.size());

		for (int i = 0, s = inTypes.size(); i < s; ++i)
		{
			Type& inType = inTypes[i];
			AST::Expression* arg = args[i];
			Type& argType = arg->getType();
			const auto result = unifyTypes(argType, inType);

			// TODO: Handle implicit casts?
			if (result == CannotUnify)
				assert("Cannot unify function argument" && false);	
		}		
	}

	void visit(AST::SymbolDeclaration* node) override
	{
		if (node->processed)
			return;
		node->processed = true;

		Symbol* symbol = node->getSymbol();

		// Check explicit type
		// TODO: How to assign "type" if inner type is always transferred?
		//	i.e var x : type; "type" needs to be a type variable that
		//	has an i¸5nner type of a type variable?
		if (node->typeExpr)
		{
			node->typeExpr->accept(this);
			const Type& type = node->typeExpr->getType();
			symbol->type = type.innerTypeFromTypeVariable();
		}

		// Infer type from init expression
		if (node->initExpr || node->isParam)
		{
			symbol->firstInitOrder = node->order;
			if (node->initExpr)
			{
				node->initExpr->accept(this);
				Type& exprType = node->initExpr->getType();
				const auto result = unifyTypes(symbol->type, exprType);

				// TODO: Handle implicit casts?
				if (result == CannotUnify)
					assert("Cannot unify types" && false);

				// TODO: How to apply unification to expression?
			}
		}
		else if (node->isExternal)
		{
			symbol->firstInitOrder = node->order;
		}

		// TODO: Resolve any type requests within this and underlying scopes
	}

	void visit(AST::SymbolExpression* node) override
	{
		if (node->processed)
			return;
		node->processed = true;

		assert(node->dependency);
		assert(node->dependency->getSymbolSource());

		// Process dependency until we are dependent on a single symbol
		//	since we are possibly dependent on a placeholder source
		while(!node->dependency->getSymbolSource()->isSingleSymbolSource())
		{
			node->dependency->getSymbolSource()->getNode()->accept(this);
		}

		// Make sure that symbol source is processed
		node->dependency->getSymbolSource()->getNode()->accept(this);

		// TODO: It does not work to use firstInitOrder with evals
		// 	since they inject code, currently getting wrongly ordered
		//	Consider doing this at a later pass, in some execution-order
		//	based traversal.
		Symbol* symbol = node->getSymbol();
		if (symbol->firstInitOrder > node->order)
		{	
			// TODO: add line/column
			printLine(string("Warning: Symbol '") + node->symbol + "' is used before initialization" + 
					"(InitOrder: " + std::to_string(symbol->firstInitOrder) + ", RefOrder: " + std::to_string(node->order) + ")");
		}
	}

	void visit(AST::EvalStatement* node) override
	{
		if (node->processed)
			return;
		node->processed = true;	

		//printLine(string("Processing eval: ") + std::to_string(node->order));

		assert(node->expr);
		assert(!node->isGenerated);
		assert(node->catchAllSource);

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

		assert(node->scopeRef);
		auto* currentScope = node->scopeRef;

		const char* text = nodeVal.data.data();
		const uint length = nodeVal.data.size();

		BufferSourceInput bufferInput(text, length);
		Parser parser(&bufferInput, currentScope);

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

		// TODO: Introduce a single node for statement lists
		// First, add all new declarations to the current scope
		for (AST::Statement* s : node->statements)
		{
			processDeclarations(s, currentScope);
		}

		//printLine("Eval statement had these dependencies:", 1);
		//for (auto d : node->catchAllSource->dependencies)
		//	printLine(d->symbolName, 2);

		// Now we can remove the catch-all source that we added earlier
		//	and hook up all the previously caught dependencies
		const bool wasRemoved = currentScope->removeSymbolSource(node->catchAllSource);
		assert(wasRemoved);
		for (auto d : node->catchAllSource->dependencies)
		{
			resolveDepdendency(d, currentScope);
		}

		//printLine(string("Resolving dependencies for ") + std::to_string(node->statements.size()) +
		//	" evaled statements..." , 1);

		// Resolve dependencies in newly created asts
		for (AST::Statement* s : node->statements)
		{
			resolveDependencies(s);
		}

		//printLine(string("Processing ") + std::to_string(node->statements.size()) +
		//	" evaled statements..." , 1);

		// Continue traversal
		for (AST::Statement* s : node->statements)
		{
			s->accept(this);
		}

		//printLine(string("Eval: ") + std::to_string(node->order) + " finished processing");
	}

};

void processAST(AST::Node* root)
{
	processDeclarations(root);
	resolveDependencies(root);

	LOG("Processing ast...");
	{
		ASTProcessor ap;
		root->accept(&ap);
	}

	bool unresolved = false;
	for (SymbolDependency* dep : s_unresolvedDependencies)
	{
		printLine(string("Could not resolve symbol ") + dep->symbolName);
		unresolved = true;
	}
	if (unresolved)
		assert(false && "Had unresolved symbols");
}

void processAST(AST::AST* ast)
{
	processAST(ast->root);
}

int main(int argc, char** argv)
{
	vector<string> args(argv + 1, argv + argc);

	if (args.size() < 1)
		ERROR("No input files specified");

	FileSourceInput fileInput(args[0]);
	AST::AST ast;

	LOG("Parsing...");
	Parser parser(&fileInput);
	bool parseSuccess = parser.parse(&ast);
	printLine("Tokens:");
	printTokens(parser.getTokens());
	if (!parseSuccess)
	{
		LOG("Parse fail!");
		printScannerErrors(parser);
		printParserErrors(parser);
	}
	else
	{
		LOG("Parse success!");
		processAST(&ast);

		printLine("AST:");
		printAST(&ast, 1);



		// Extract filename without path
		std::regex filenameRegex(R"((.*[\\\/])?(.+)$))");
		std::smatch matches;
		assert(std::regex_search(args[0], matches, filenameRegex));

		string outFileName = string(".smug/") + matches[2].str();

		/*
		std::stringstream output;
		CGenerator generator(&output);
		generator.run(&ast);
		{
			string outCFileName = outFileName + ".c";
			std::ofstream outFile(outCFileName);

			printLine("Generated C:");
			string l;
			while (getline(output, l))
			{
				printLine(l, 1);
				outFile << l << std::endl;
			}
		}
		*/
		
		std::stringstream llvmOutput;
		LLVMIRGenerator llvmgenerator(&llvmOutput);
		llvmgenerator.run(&ast);
		{
			string outLLVMFileName = outFileName + ".ll";
			std::ofstream outFile(outLLVMFileName);

			printLine("Generated LLVM IR:");
			string l;
			while (getline(llvmOutput, l))
			{
				printLine(l, 1);
				outFile << l << std::endl;
			}
		}

		// output generic filename without ext
		std::cout << outFileName;
	}
}

