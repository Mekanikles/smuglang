#pragma once

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
#include "llvm/IR/Type.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/Host.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/TargetRegistry.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Target/TargetOptions.h"

#include "llvm/Support/raw_ostream.h"

static llvm::LLVMContext s_theContext;
static llvm::IRBuilder<> s_builder(s_theContext);
static std::unique_ptr<llvm::Module> s_theModule = llvm::make_unique<llvm::Module>("SmugModule", s_theContext);;
static std::map<string, llvm::Value*> s_namedValues;

struct LLVMIRGenerator : AST::Visitor
{
	using AST::Visitor::visit;
	void visit(AST::StringLiteral* node) override
	{ 
		auto s = llvm::StringRef(node->value);
		m_valueStack.push_back(llvm::ConstantDataArray::getString(m_context, s, false));
	}
	void visit(AST::IntegerLiteral* node) override 
	{ 	
		auto s = llvm::StringRef(node->value);
		const Type& t = node->getType();
		const auto& primitive = t.getPrimitive();
		assert(primitive.isInteger());

		//assert(primitive.knowsSize() && primitive.knowsSign()); // TODO: Introduce defaults later
		auto size = primitive.knowsSize() ? primitive.size : DEFAULT_INT_SIZE;
		//auto isSigned = primitive.knowsSign() ? primitive.isSigned() : DEFAULT_INT_ISSIGNED;

		auto iType = llvm::IntegerType::get(m_context, size);
		// TODO: Make sure we don't allow stupid octal syntax
		auto val = llvm::ConstantInt::get(iType, s, 10);
		m_valueStack.push_back(val);
	}
	void visit(AST::FloatLiteral* node) override
	{
		m_valueStack.push_back(llvm::ConstantFP::get(m_context, 
				llvm::APFloat(llvm::APFloatBase::IEEEsingle(), llvm::StringRef(node->value))));
	}
	void visit(AST::TypeLiteral* node) override
	{
		visit((AST::Expression*)node);
		assert(false && "hmmm");
	}

	void visit(AST::BinaryOp* node) override
	{	
		auto* leftExpr = node->left;
		auto* rightExpr = node->right;
		const auto& type = node->getType();

		leftExpr->accept(this);
		auto leftVal = m_valueStack.back(); 
		m_valueStack.pop_back();

		rightExpr->accept(this);
		auto rightVal = m_valueStack.back(); 
		m_valueStack.pop_back();

		assert(type.isPrimitive());
		const auto& primitive = type.getPrimitive();
		if (primitive.isInteger() || primitive.isChar())
		{
			switch (node->opType)
			{
				case TokenType::CompareOp: 
					m_valueStack.push_back(m_builder.CreateICmpEQ(leftVal, rightVal, "icmp")); 
					break;
				case TokenType::AddOp:					
					m_valueStack.push_back(m_builder.CreateAdd(leftVal, rightVal, "iadd")); 
					break;
				case TokenType::SubtractOp:					
					m_valueStack.push_back(m_builder.CreateSub(leftVal, rightVal, "isub")); 
					break;
				case TokenType::MultiplicationOp:					
					m_valueStack.push_back(m_builder.CreateMul(leftVal, rightVal, "imul")); 
					break;
				case TokenType::DivisionOp:					
					m_valueStack.push_back(m_builder.CreateSDiv(leftVal, rightVal, "idiv")); 
					break;
				default: assert(false);
			}
		}
		else
		{
			switch (node->opType)
			{
				case TokenType::CompareOp: 
					m_valueStack.push_back(m_builder.CreateFCmpOEQ(leftVal, rightVal, "fcmp")); 
					break;
				case TokenType::AddOp:					
					m_valueStack.push_back(m_builder.CreateFAdd(leftVal, rightVal, "fadd")); 
					break;
				case TokenType::SubtractOp:					
					m_valueStack.push_back(m_builder.CreateFSub(leftVal, rightVal, "fsub")); 
					break;
				case TokenType::MultiplicationOp:					
					m_valueStack.push_back(m_builder.CreateFMul(leftVal, rightVal, "fmul")); 
					break;
				case TokenType::DivisionOp:					
					m_valueStack.push_back(m_builder.CreateFDiv(leftVal, rightVal, "fdiv")); 
					break;
				default: assert(false);
			}
		}
	}

	void visit(AST::StatementBody* node) override
	{
		AST::Visitor::visit(node);

	}

	LLVMIRGenerator(std::ostream* out)
		: m_out(out)
		, m_module(*s_theModule)
		, m_context(s_theContext)
		, m_builder(s_builder)		
	{
	}

	void run(AST::AST* ast)
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

		ast->root->accept(this);

		if (m_valueStack.size() > 0)
		{
			print("Warning: found unused values: ");
			printLine(std::to_string(m_valueStack.size()));
			// Temp
			m_builder.CreateRet(m_valueStack.back());
		}

		verifyFunction(*mainFunc);

		// On error
		//TheFunction->eraseFromParent();

		verifyModule(m_module);
		llvm::legacy::PassManager passManager;
		passManager.add(llvm::createPrintModulePass(llvm::errs()));
		passManager.run(m_module);
	}

	std::ostream* m_out;
	llvm::Module& m_module;
	llvm::LLVMContext& m_context;
	llvm::IRBuilder<> m_builder;
	std::vector<llvm::Value*> m_valueStack;	
};
