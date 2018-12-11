#pragma once
#include "core.h"
#include "value.h"
#include "ir.h"

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

namespace Backend
{

bool g_backendInitialized = false;
void ensureBackendIsInitialized()
{
	// Init llvm stuff
	if (!g_backendInitialized)
	{
		llvm::InitializeAllTargetInfos();
		llvm::InitializeAllTargets();
		llvm::InitializeAllTargetMCs();
		llvm::InitializeAllAsmParsers();
		llvm::InitializeAllAsmPrinters();
		g_backendInitialized = true;
	}
}

struct Context
{
	Value* createIntegerValueFromText(string text, int size, bool isSigned)
	{
		auto iType = llvm::IntegerType::get(m_llvmContext, size);
		// TODO: Make sure we don't allow stupid octal syntax
		auto val = llvm::ConstantInt::get(iType, llvm::StringRef(text), 10);
		return val;
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
				auto iType = llvm::IntegerType::get(m_llvmContext, size);
				return iType;
			}
		}

		assert("Cannot resolve type" && false);
		return nullptr;
	}

	llvm::Function* createFunction(IR::Function& irfunction)
	{
		assert(irfunction.getType()->isConcrete() && "Inconcrete types are not allowed in ir generation");

		vector<string> paramNames;
		std::vector<llvm::Type*> args;

		for (IR::Param& param : irfunction.signature.inParams)
		{
			const TypeRef& type = param.getType();

			int paramIndex = args.size();
			args.push_back(resolveType(type.getType()));
			paramNames.push_back(param.getName());

			// Hm, why is this assigned here AND when creating the param?
			param.index = paramIndex;
		}

		// TODO: multiple return values
		llvm::Type* returnType = m_llvmBuilder.getVoidTy();
		if (!irfunction.signature.outParams.empty())
		{
			assert(irfunction.signature.outParams.size() == 1);
			returnType = resolveType(irfunction.signature.outParams[0].getType());
		}

		auto functionType = llvm::FunctionType::get(
				returnType, args, irfunction.isVariadic);

		auto func = llvm::Function::Create(functionType, llvm::Function::ExternalLinkage, irfunction.name, &*m_llvmModule);

		// Assign parameter names
		uint i = 0;
		for (auto& a : func->args())
			a.setName(paramNames[i++]);

		return func;
	}

	void printIR(std::ostream& out)
	{
		llvm::legacy::PassManager passManager;
		llvm::raw_os_ostream ostr(out);
		passManager.add(llvm::createPrintModulePass(ostr));
		passManager.run(*m_llvmModule);
	}

	Context()
		: m_llvmBuilder(m_llvmContext)
		, m_llvmModule(llvm::make_unique<llvm::Module>("TheModule", m_llvmContext))
	{
		ensureBackendIsInitialized();

		// Set up target machine and configure module 
		{
			auto targetTriple = llvm::sys::getDefaultTargetTriple();
			m_llvmModule->setTargetTriple(targetTriple);

			std::string errorString;
			auto target = llvm::TargetRegistry::lookupTarget(targetTriple, errorString);

			// Print an error and exit if we couldn't find the requested target.
			// This generally occurs if we've forgotten to initialise the
			// TargetRegistry or we have a bogus target triple.
			if (!target) 
			{
				llvm::errs() << "Target not found";
				assert(false);
			}

			auto targetCPU = "generic";
			auto targetFeatures = "";

			llvm::TargetOptions targetOptions;
			auto targetModel = llvm::Reloc::Model();
			auto targetMachine = target->createTargetMachine(targetTriple, 
					targetCPU, targetFeatures, targetOptions, targetModel);

			m_llvmModule->setDataLayout(targetMachine->createDataLayout());
		}
	}

	llvm::LLVMContext m_llvmContext;
	llvm::IRBuilder<llvm::NoFolder> m_llvmBuilder;
	std::unique_ptr<llvm::Module> m_llvmModule;	
};

struct Generator
{
	void generateStatement(IR::Statement& irstatement)
	{
		switch (irstatement.statementType)
		{
		case IR::Statement::Scope:
		{
			auto* scope = static_cast<IR::Scope*>(&irstatement);
			generateScope(*scope);
			break;
		}

		case IR::Statement::Assignment:
		{
			//auto* assignment = static_cast<IR::Assignment*>(&irstatement);
			break;
		}

		case IR::Statement::Call:
		{
			//auto* call = static_cast<IR::Call*>(&irstatement);
			//for (auto& arg : call->args)	
			break;
		}

		case IR::Statement::Conditional:
		{
			//auto* conditional = static_cast<IR::Conditional*>(&irstatement);	
			break;
		}
		}

		/*auto previousBlock = m_context.m_llvmBuilder.GetInsertBlock();
		auto entryBlock = llvm::BasicBlock::Create(m_context.m_llvmContext, "entry", func);
		m_context.m_llvmBuilder.SetInsertPoint(entryBlock);
		*/

		//assert(node->body);
		//node->body->accept(this);
	}

	void generateBlock(IR::Block& irblock)
	{
		for (auto& statement : irblock.statements)
		{
			generateStatement(*statement);
		}
	}

	void generateScope(IR::Scope& irscope)
	{
		vector<IR::Function*> funcs;

		// Generate referenceables
		for (auto& ref : irscope.referenceables)
		{	
			auto& val = ref->value;
			assert(val);
			switch (val->valueType)
			{
				case IR::Value::Variable:
				{
					auto* var = static_cast<IR::Variable*>(ref->value.get());
					
					auto* type = m_context.resolveType(var->getType());
					assert(!var->backendValue);
					var->backendValue = m_context.m_llvmBuilder.CreateAlloca(type, nullptr, ref->getName());
					break;
				}
				case IR::Value::Function:
				{
					auto* func = static_cast<IR::Function*>(ref->value.get());
					// Only generate heads now, will allow for cross-referencing functions
					generateFunctionHead(*func);
					funcs.push_back(func);
					break;
				}
				// TODO: Why can this be here? Rename referenceables to declarations?
				//	reffables can be unnamed expressions/lambdas etc.
				case IR::Value::Expression:
				{
					assert(false && "Hm, why can we have expressions in scope referenceables?");
					break;
				}
			}
		}

		// Generate all function bodies
		for (auto* func : funcs)
		{	
			generateFunctionBody(*func);
		}

		// Generate all blocks
		for (auto& block : irscope.blocks)
		{
			generateBlock(*block);
		}
	}

	void generateFunctionHead(IR::Function& irfunction)
	{
		auto* func = m_context.createFunction(irfunction);
		assert(!irfunction.backendValue);
		irfunction.backendValue = func;
	}

	void generateFunctionBody(IR::Function& irfunction)
	{
		if (irfunction.external)
			return;

		assert(irfunction.backendValue);
		auto* func = static_cast<llvm::Function*>(irfunction.backendValue);

		auto entryBlock = llvm::BasicBlock::Create(m_context.m_llvmContext, "entry", func);
		m_context.m_llvmBuilder.SetInsertPoint(entryBlock);

		// Link signature variables to function argument value
		auto& signature = irfunction.signature;
		uint argIndex = 0;
		for (auto& arg : func->args())
		{
			// Ugh, search for param with this index
			IR::Param* paramPtr = nullptr;
			for (IR::Param& param : signature.inParams)
			{
				if (param.index == argIndex)
				{
					paramPtr = &param;
					break;
				}
			}

			// TODO: Point const params to argument directly
			auto a = m_context.m_llvmBuilder.CreateAlloca(arg.getType(), nullptr, arg.getName());

			// TODO: This is not pretty, params should only be variables 
			//	(constants should not be allowed for a concretized function, right?)
			assert(paramPtr->value->valueType == IR::Value::Variable);
			paramPtr->value->backendValue = m_context.m_llvmBuilder.CreateStore(&arg, a);

			argIndex++;
		}

		generateScope(irfunction.scope);

		// HACK: Append emtpy ret on void functions to make llvm happy
		auto& functionClass = irfunction.getType()->getFunction();		
		if (functionClass.outParams.empty())
			m_context.m_llvmBuilder.CreateRetVoid();

		verifyFunction(*func);
	}

	void generateModule(IR::Module& irmodule)
	{
		generateFunctionHead(*irmodule.mainFunction);
		generateFunctionBody(*irmodule.mainFunction);
	}

	Generator(Context& context)
		: m_context(context)
	{
	}

	Context& m_context;
};

}