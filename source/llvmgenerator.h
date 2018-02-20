#pragma once

#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/CallingConv.h"
#include "llvm/IR/Verifier.h"
//#include "llvm/IR/PassManager.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/IR/IRPrintingPasses.h"

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
		// TODO: Radix 0 parses hex and binary, but allows stupid octal syntax, which we dont want
		m_valueStack.push_back(llvm::ConstantInt::get(iType, s, 0));
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
		/* TODO: Add main function
		; Function Attrs: norecurse nounwind readnone ssp uwtable
		define i32 @main(i32, i8** nocapture readnone) local_unnamed_addr #0 {
		  ret i32 42
		}*/


		ast->root->accept(this);

		print("Number of values in stack: ");
		printLine(std::to_string(m_valueStack.size()));

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
