#pragma once

#include <unordered_map>

#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/Optional.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/IRPrintingPasses.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/NoFolder.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/Host.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/raw_os_ostream.h"
#include "llvm/Support/TargetRegistry.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Target/TargetOptions.h"

#include "llvm/Support/raw_ostream.h"

const uint DEFAULT_INT_SIZE = 32;
const bool DEFAULT_INT_ISSIGNED = true;

static llvm::LLVMContext s_theContext;
static llvm::IRBuilder<llvm::NoFolder> s_builder(s_theContext);
static std::unique_ptr<llvm::Module> s_theModule = llvm::make_unique<llvm::Module>("SmugModule", s_theContext);
static std::map<string, llvm::Value*> s_namedValues;

struct LLVMIRGenerator : AST::Visitor
{
	llvm::Type* getFloatType(int size)
	{
		switch (size)
		{
			case 16:
				return llvm::Type::getHalfTy(m_context);
				break;
			case 32:
				return llvm::Type::getFloatTy(m_context);
				break;
			case 64:
				return llvm::Type::getDoubleTy(m_context);
				break;
			case 128:
				return llvm::Type::getFP128Ty(m_context);
				break;
			default:
				assert(false && "Bad float size");							
		}
		return nullptr;
	}

	llvm::Type* resolveType(const Type& type)
	{
		if (type.isPointer())
		{
			const auto& pointer = type.getPointer();
			auto pType = resolveType(pointer.type.getType())->getPointerTo();
			return pType;
		}
		else if (type.isPrimitive())
		{
			const auto& primitive = type.getPrimitive();
			if (primitive.isInteger() || primitive.isChar())
			{
				auto size = primitive.knowsSize() ? primitive.size : DEFAULT_INT_SIZE;
				auto iType = llvm::IntegerType::get(m_context, size);
				return iType;
			}
			else
			{
				auto size = primitive.knowsSize() ? primitive.size : DEFAULT_INT_SIZE;
				return getFloatType(size);
			}
		}

		assert("Cannot resolve type" && false);
		return nullptr;
	}

	using AST::Visitor::visit;
	void visit(AST::StringLiteral* node) override
	{ 
		/*auto s = llvm::StringRef(node->value);
		auto zero = llvm::ConstantInt::get(llvm::Type::getInt8Ty(m_context), 0);
		llvm::Value* indexList[2] = {zero, zero};
		// TODO: Do not null terminate strings
		m_valueStack.push_back(m_builder.CreateGEP(
				llvm::ConstantDataArray::getString(m_context, s, true),
				indexList));*/
		const string str = processQuotedInputString(node->value);
		pushValue(m_builder.CreateGlobalStringPtr(str.c_str()));
	}
	void visit(AST::IntegerLiteral* node) override 
	{ 	
		auto s = llvm::StringRef(node->value);
		const Type& t = node->getType(m_astContext);

		const auto& primitive = t.getPrimitive();
		assert(primitive.isInteger());

		//assert(primitive.knowsSize() && primitive.knowsSign()); // TODO: Introduce defaults later
		auto size = primitive.knowsSize() ? primitive.size : DEFAULT_INT_SIZE;
		//auto isSigned = primitive.knowsSign() ? primitive.isSigned() : DEFAULT_INT_ISSIGNED;

		auto iType = llvm::IntegerType::get(m_context, size);
		// TODO: Make sure we don't allow stupid octal syntax
		auto val = llvm::ConstantInt::get(iType, s, 10);
		pushValue(val);
	}
	void visit(AST::FloatLiteral* node) override
	{
		pushValue(llvm::ConstantFP::get(m_context, 
				llvm::APFloat(llvm::APFloatBase::IEEEsingle(), llvm::StringRef(node->value))));
	}
	void visit(AST::TypeLiteral* node) override
	{
		// TODO: Prune pure type stuff before ir generation
	}

	llvm::Function* createFunction(const Type& type, const string& name, bool isExternal = false)
	{
		assert(type.isFunction());
		const FunctionClass& function = type.getFunction();

		vector<string> paramNames;

		bool isCVariadic = false;
		std::vector<llvm::Type*> args;
		for (int i = 0, e = function.inParams.size(); i < e; ++i)
		{
			const auto& param = function.inParams[i];
			const TypeRef& t = param.type;

			if (t->isTuple())
			{
				auto& tuple = t->getTuple();

				// TODO: Make sure CVariadics can only have one tuple at the end
				//	Check this in the parser?
				if (tuple.unbounded)
				{
					if (isExternal)
					{
						// TODO: Handle bounded tuples?
						isCVariadic = true;
						assert(i == e - 1 && "External function cannot have more than one unbounded tuple");
						break;	
					}
					else
					{
						assert("Unbounded tuples are not allowed for non-externals");
					}
				}
				else
				{
					assert(t->isConcrete() && "Inconcrete types are not allowed in ir generation");

					uint count = 0;
					for (const TypeRef& tt : tuple.types)
					{
						assert(!tt->isTuple() && "Nested tuple types not allowed");

						args.push_back(resolveType(tt.getType()));
						string id = param.identifier;
						id += ".";
						id += std::to_string(count++);
						paramNames.push_back(id);
					}
				}
			}
			else
			{
				assert(t->isConcrete() && "Inconcrete types are not allowed in ir generation");

				args.push_back(resolveType(t.getType()));
				paramNames.push_back(param.identifier);
			}
		}

		// TODO: multiple return values
		llvm::Type* returnType = m_builder.getVoidTy();
		if (!function.outParams.empty())
		{
			assert(function.outParams.size() == 1);
			returnType = resolveType(function.outParams[0].type);
		}

		auto functionType = llvm::FunctionType::get(
			returnType, args, isCVariadic);

		auto func = llvm::Function::Create(functionType, llvm::Function::ExternalLinkage, name, &m_module);

		// Assign parameter names
		uint i = 0;
		for (auto& a : func->args())
			a.setName(paramNames[i++]);

		return func;
	}

	llvm::Function* processFunctionLiteral(AST::FunctionLiteral* node, const string& name)
	{
		auto func = createFunction(node->getType(m_astContext), name);
		m_functionLiterals[node] = func;

		auto previousBlock = m_builder.GetInsertBlock();
		auto entryBlock = llvm::BasicBlock::Create(m_context, "entry", func);
		m_builder.SetInsertPoint(entryBlock);

		// Create storage for all parameters
		// TODO: Necessary for non mutable params?
		for (auto& arg : func->args())
		{
			auto a = m_builder.CreateAlloca(arg.getType(), nullptr, arg.getName());
			m_builder.CreateStore(&arg, a);
			m_variables[arg.getName()] = a;
		}

		m_foundReturnStatement = false;
		assert(node->body);
		node->body->accept(this);

		if (!m_foundReturnStatement)
			m_builder.CreateRetVoid();

		verifyFunction(*func);

		// Restore any previous basic block
		m_builder.SetInsertPoint(previousBlock);

		return func;	
	}

	void visit(AST::FunctionDeclaration* node) override
	{
		Symbol* symbol = node->getSymbol(m_astContext);

		auto func = m_functions[symbol];
		if (func == nullptr)
		{
			auto funcLiteral = node->funcLiteral;
			assert(funcLiteral);			
			func = processFunctionLiteral(funcLiteral, symbol->name);
			m_functions[symbol] = func;
		}

		pushValue(func);
	}

	void visit(AST::SymbolDeclaration* node) override
	{
		// TODO: process AST into strongly typed AST before IR and get rid of defs
		// Definitions does not generate any code
		if (node->isDefine())
			return;
		
		Symbol* symbol = node->getSymbol(m_astContext);
		const Type& type = symbol->type;

		if (type.isFunction())
		{
			assert(node->isExternal());
			auto func = m_functions[symbol];
			if (func == nullptr)
			{
				func = createFunction(type, symbol->name, node->isExternal()); 
				m_functions[symbol] = func;
			}

			pushValue(func);
		}
		else
		{
			// TODO: Prune pure type stuff before generating ir
			assert(!type.isTypeVariable());

			auto variable = m_variables[symbol->name];
			if (variable == nullptr)
			{
				auto t = resolveType(type);
				variable = m_builder.CreateAlloca(t, nullptr, symbol->name.c_str());

				if (node->initExpr)
				{
					const int valueCount = m_valueStack.size();
					node->initExpr->accept(this);
					assert(m_valueStack.size() == valueCount + 1);

					//auto loadInst = m_builder.407(popValue());
					m_builder.CreateStore(popValue(), variable);
				}

				m_variables[symbol->name] = variable;
			}
			pushValue(variable);
		}
	}

	void visit(AST::Assignment* node) override
	{
		// NOTE: Do NOT evaluate symbol expr, we don't want a load here
		// TODO: Handle expressions that return assignable "values"
		assert(node->symExpr);
		Symbol* symbol = node->symExpr->getSymbol(m_astContext);

		// TODO: This relies on decl node being parsed first,
		// 	make sure IR is arranged in correct order
		auto var = m_variables[symbol->name];
		assert(var);

		assert(node->expr);
		const int valueCount = m_valueStack.size();	
		node->expr->accept(this);
		assert(m_valueStack.size() == valueCount + 1);

		//auto loadInst = m_builder.CreateLoad(popValue());
		m_builder.CreateStore(popValue(), var);
	}

	void visit(AST::ReturnStatement* node) override
	{
		llvm::Value* val = nullptr;
		if (node->expr)
		{
			node->expr->accept(this);

			val = popValue();
		}

		m_foundReturnStatement = true;
		m_builder.CreateRet(val);
	}

	// TODO: Generalize into declaration
	void visit(AST::FunctionInParam* node) override
	{
		assert(!node->initExpr);

		// We have already stored in-args in the function literal
		auto symbol = node->getSymbol(m_astContext);
		auto val = m_variables[symbol->name];
		assert(val);
		pushValue(val);
	}

	void visit(AST::SymbolExpression* node) override
	{
		auto dependencyNode = node->getNodeForDependency(m_astContext);
		dependencyNode->accept(this);

		Symbol* symbol = node->getSymbol(m_astContext);

		const TypeRef& type = symbol->getType();
		if (type->isTuple())
		{
			auto& tuple = type->getTuple();
			for (uint i = 0, s = tuple.types.size(); i < s; ++i)
			{
				auto var = popValue();
				assert(var && "Could not find declared symbol variable");

				pushValue(var);

				//auto loadInst = m_builder.CreateLoad(var);
				//m_valueStack.push_back(loadInst);
			}
		}
		else if (type->isFunction())
		{
			auto func = popValue();
			assert(func && "Could not find declared symbol function");

			pushValue(func);

			//auto loadInst = m_builder.CreateLoad(func);

			// TODO: Here we push a second value on the stack without using the current
			// 	one pushed by the expr eval
			//m_valueStack.push_back(loadInst);
		}
		else
		{
			auto var = popValue();
			assert(var && "Could not find declared symbol variable");

			auto loadInst = m_builder.CreateLoad(var);
			pushValue(loadInst);
		}
	}

	void visit(AST::Call* node) override
	{
		assert(node->expr);
		node->expr->accept(this);

		// TODO: SymbolExpession generates too many values
		assert(m_valueStack.size() > 0);
		llvm::Value* llvmValue = popValue();
		// TODO: should probably not allow pointer types for compile-time generated code
		//	These will point into compiler memory and cannot be treated as constants without
		//	some kind of transformation first.
		llvmValue = llvmValue->stripPointerCasts();

		// TODO: How to get inner type of pointer value
		//assert(llvmValue->getType()->getTypeID() == llvm::Type::FunctionTyID);
		auto func = static_cast<llvm::Function*>(llvmValue);

		const int prevValCount = m_valueStack.size();

		std::vector<llvm::Value*> args;
		for (auto* expr : node->args)
		{
			expr->accept(this);

			// Flatten tuples
			uint expectedValCount = 1;
			const TypeRef& type = expr->getType(m_astContext);
			if (type->isTuple())
			{
				auto& tuple = type->getTuple();
				assert(!tuple.unbounded);
				expectedValCount = tuple.types.size();
			}

			assert(m_valueStack.size() == prevValCount + expectedValCount);
			for (uint i = 0; i < expectedValCount; ++i)
			{
				//auto loadInst = m_builder.CreateLoad(popValue());
				args.push_back(popValue());		
			}
		}

		llvm::Value* retVal = m_builder.CreateCall(func, args);
		pushValue(retVal);
	}

	void visit(AST::BinaryOp* node) override
	{	
		auto* leftExpr = node->left;
		auto* rightExpr = node->right;
		const auto& type = node->getType(m_astContext);

		leftExpr->accept(this);
		auto leftVal = popValue();

		rightExpr->accept(this);
		auto rightVal = popValue();

		assert(type->isPrimitive());
		const auto& primitive = type->getPrimitive();
		if (primitive.isInteger() || primitive.isChar())
		{
			switch (node->opType)
			{
				case TokenType::CompareOp:
					pushValue(m_builder.CreateICmpEQ(leftVal, rightVal, "icmp")); 
					break;
				case TokenType::Plus:
					pushValue(m_builder.CreateAdd(leftVal, rightVal, "iadd")); 
					break;
				case TokenType::Minus:
					pushValue(m_builder.CreateSub(leftVal, rightVal, "isub")); 
					break;
				case TokenType::Asterisk:
					pushValue(m_builder.CreateMul(leftVal, rightVal, "imul")); 
					break;
				case TokenType::Slash:
					pushValue(m_builder.CreateSDiv(leftVal, rightVal, "idiv")); 
					break;
				default: assert(false);
			}
		}
		else
		{
			switch (node->opType)
			{
				case TokenType::CompareOp:
					pushValue(m_builder.CreateFCmpOEQ(leftVal, rightVal, "fcmp")); 
					break;
				case TokenType::Plus:
					pushValue(m_builder.CreateFAdd(leftVal, rightVal, "fadd")); 
					break;
				case TokenType::Minus:
					pushValue(m_builder.CreateFSub(leftVal, rightVal, "fsub")); 
					break;
				case TokenType::Asterisk:
					pushValue(m_builder.CreateFMul(leftVal, rightVal, "fmul")); 
					break;
				case TokenType::Slash:
					pushValue(m_builder.CreateFDiv(leftVal, rightVal, "fdiv")); 
					break;
				default: assert(false);
			}
		}
	}

	void visit(AST::StatementBody* node) override
	{
		auto prevStackSize = m_valueStack.size();

		for (AST::Statement* statement : node->statements)
		{
			statement->accept(this);
			auto stackSize = m_valueStack.size();
			assert(stackSize - prevStackSize < 2);
			// Allow "dangling" values
			if (stackSize - prevStackSize == 1)
				popValue();
		}
	}

	void printValue(llvm::Value* val, int indent = 0)
	{
		std::stringstream llvmOutput;
		llvm::raw_os_ostream llvmOut(llvmOutput);
		val->print(llvmOut);
		printLine(llvmOutput.str(), indent);
	}

	void pushValue(llvm::Value* val)
	{
		m_valueStack.push_back(val);
		//printLine("Pushed value, stack size: " + std::to_string(m_valueStack.size()));
		//printValue(val, 1);
	}

	llvm::Value* popValue()
	{
		assert(m_valueStack.size() > 0);
		auto retVal = m_valueStack.back();
		m_valueStack.pop_back();
		//printLine("Popped value, stack size: " + std::to_string(m_valueStack.size()));
		return retVal;
	}

	LLVMIRGenerator(ASTContext* context, std::ostream* out, 
		llvm::Module* llvmModule, llvm::LLVMContext* llvmContext, 
		llvm::IRBuilder<llvm::NoFolder>* llvmBuilder)
		: m_astContext(context)
		, m_out(*out)
		, m_module(*llvmModule)
		, m_context(*llvmContext)
		, m_builder(*llvmBuilder)		
	{
	}

	void run(AST::ASTObject* ast)
	{
		// Init llvm stuff
		{
			llvm::InitializeAllTargetInfos();
			llvm::InitializeAllTargets();
			llvm::InitializeAllTargetMCs();
			llvm::InitializeAllAsmParsers();
			llvm::InitializeAllAsmPrinters();
		}

		// Set up target machine and configure module 
		{
			auto targetTriple = llvm::sys::getDefaultTargetTriple();
			m_module.setTargetTriple(targetTriple);

			std::string errorString;
			auto target = llvm::TargetRegistry::lookupTarget(targetTriple, errorString);

			// Print an error and exit if we couldn't find the requested target.
			// This generally occurs if we've forgotten to initialise the
			// TargetRegistry or we have a bogus target triple.
			if (!target) 
			{
				llvm::errs() << "Target not found";
				return;
			}

			auto targetCPU = "generic";
			auto targetFeatures = "";

			llvm::TargetOptions targetOptions;
			auto targetModel = llvm::Reloc::Model();
			auto targetMachine = target->createTargetMachine(targetTriple, 
					targetCPU, targetFeatures, targetOptions, targetModel);

			m_module.setDataLayout(targetMachine->createDataLayout());
		}

		/* TODO: Add main function
		; Function Attrs: norecurse nounwind readnone ssp uwtable
		define i32 @main(i32, i8** nocapture readnone) local_unnamed_addr #0 {
		  ret i32 42
		}*/

		// Construct args (int argc, char**)
		std::vector<llvm::Type*> mainArgs;
		mainArgs.push_back(llvm::Type::getInt32Ty(m_context));
		mainArgs.push_back(llvm::Type::getInt8PtrTy(m_context)->getPointerTo());

		// Construct main function type
		auto mainType = llvm::FunctionType::get(
				llvm::Type::getInt32Ty(m_context), mainArgs, false);

		auto mainFunc = llvm::Function::Create(mainType, llvm::Function::ExternalLinkage, "main", &m_module);

		// Name args for easier debugging
		const char* argNames[2] = { "argc", "argv" };
		uint i = 0;
		for (auto& a : mainFunc->args())
			a.setName(argNames[i++]);

		auto basicBlock = llvm::BasicBlock::Create(m_context, "entry", mainFunc);
		m_builder.SetInsertPoint(basicBlock);

		ast->module->accept(this);

		// TODO: Hm, I don't think the valuestack can be used like
		//	this, a statement is allowed to "dangle" values
		/*int foundUnusedValues = 0;
		for (auto* val : m_valueStack)
		{
			// TODO: Check for unused values	
			if (val)
				foundUnusedValues++;
		}
		if (foundUnusedValues > 0)
		{
			print("Warning: found unused values: ");
			printLine(std::to_string(foundUnusedValues));
		}*/

		// Main exit code
		m_builder.CreateRet(llvm::ConstantInt::get(llvm::Type::getInt32Ty(m_context), 0));

		verifyFunction(*mainFunc);

		// On error
		//TheFunction->eraseFromParent();

		verifyModule(m_module);
		llvm::legacy::PassManager passManager;
		passManager.add(llvm::createPrintModulePass(m_out));
		passManager.run(m_module);
	}

	ASTContext* m_astContext;

	llvm::raw_os_ostream m_out;
	llvm::Module& m_module;
	llvm::LLVMContext& m_context;
	llvm::IRBuilder<llvm::NoFolder> m_builder;
	std::vector<llvm::Value*> m_valueStack;

	// TODO: Terrible hack, we need to do flow analysis for return statements
	bool m_foundReturnStatement;

	std::unordered_map<Symbol*, llvm::Function*> m_functions;
	std::unordered_map<AST::FunctionLiteral*, llvm::Function*> m_functionLiterals;

	std::unordered_map<string, llvm::AllocaInst*> m_variables;
};

LLVMIRGenerator* createGenerator(ASTContext* astcontext, std::ostream* out)
{
	return new LLVMIRGenerator(astcontext, out, s_theModule.get(), &s_theContext, &s_builder);
}


