
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
#include "context.h"
#include "ast.h"
#include "functions.h"
#include "parser.h"
#include "output.h"
#include "llvmgenerator.h"

#include "declarationprocessor.h"
#include "dependencyresolver.h"

#include "evaluation.h"
#include "astprocessor.h"

///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////

int main(int argc, char** argv)
{
	vector<string> args(argv + 1, argv + argc);

	if (args.size() < 1)
		ERROR("No input files specified");

	FileSourceInput fileInput(args[0]);
	AST::ASTObject ast;

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
		Context astContext;

		assert(initExpressionEvaluator());

		processAST(&astContext, &ast);

		printLine("Compiled AST:");
		printAST(&astContext, &ast, 1);

		// Extract filename without path
		std::regex filenameRegex(R"((.*[\\\/])?(.+)$))");
		std::smatch matches;
		assert(std::regex_search(args[0], matches, filenameRegex));

		string outFileName = string(".smug/") + matches[2].str();

		std::stringstream llvmOutput;
		LLVMIRGenerator* llvmgenerator = createGenerator(&astContext, &llvmOutput);
		llvmgenerator->run(&ast);
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

