#include "core.h"
#include "utils.h"
#include "input.h"
#include "token.h"
#include "scanner.h"

#include "types.h"
#include "symbols.h"
#include "context.h"
#include "ast.h"
#include "functions.h"
#include "parser.h"
#include "backend/backend.h"
#include "ir/ir.h"
#include "output.h"
#include "concretization.h"
#include "llvmgenerator.h"

#include "declarationprocessor.h"
#include "dependencyresolver.h"

#include "evaluation/evaluation.h"
#include "astprocessor.h"

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
	if (false)
	{
		printLine("Tokens:");
		printTokens(parser.getTokens());
	}
	if (!parseSuccess)
	{
		LOG("Parse fail!");
		printScannerErrors(parser);
		printParserErrors(parser);
	}
	else
	{
		LOG("Parse success!");
		ASTContext astContext("mainAST");

		Backend::ensureBackendIsInitialized();
		Backend::Context backend;

		Backend::Context evaluationBackend;
		IR::Module evaluationIrModule;
		EvaluationContext econtext { &evaluationBackend, &evaluationIrModule };

		assert(Evaluation::init(econtext));

		printLine("Processing AST:");
		processAST(econtext, &astContext, &ast);

		printLine("Resulting AST:");
		printAST(&astContext, &ast, 1);

		printLine("Generating concrete MIR from AST...");
		IR::Module irModule = concretizeASTModule(&backend, &astContext, ast.module);
		printLine("Generated MIR:");
		printIRModule(&irModule);
	
		// Extract filename without path
		std::regex filenameRegex(R"((.*[\\\/])?(.+)$))");
		std::smatch matches;
		assert(std::regex_search(args[0], matches, filenameRegex));
		string outFileName = string(".smug/") + matches[2].str();

		printLine("Generating IR from MIR...");
		{
			Backend::Generator generator(backend);
			generator.generateModule(irModule);
			std::stringstream backendOutput;
			backend.printIR(backendOutput);

			string outLLVMFileName = outFileName + ".ll";
			std::ofstream outFile(outLLVMFileName);		

			printLine("Generated IR:");
			string l;
			while (getline(backendOutput, l))
			{
				printLine(l, 1);
				outFile << l << std::endl;
			}
		}

		/*
		printLine("Generating LLVM IR from AST...");
		{
			std::stringstream llvmOutput;
			LLVMIRGenerator* llvmgenerator = createGenerator(&astContext, &llvmOutput);
			llvmgenerator->run(&ast);

			//string outLLVMFileName = outFileName + ".ll";
			//std::ofstream outFile(outLLVMFileName);

			printLine("Generated LLVM IR:");
			string l;
			while (getline(llvmOutput, l))
			{
				printLine(l, 1);
				//outFile << l << std::endl;
			}
		}*/

		// output generic filename without ext
		std::cout << outFileName;
	}
}

