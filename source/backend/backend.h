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

static const llvm::fltSemantics* getFloatStandard(int size)
{
	switch (size)
	{
		case 16:
			return &llvm::APFloatBase::IEEEhalf();
			break;
		case 32:
			return &llvm::APFloatBase::IEEEsingle();
			break;
		case 64:
			return &llvm::APFloatBase::IEEEdouble();
			break;
		case 128:
			return &llvm::APFloatBase::IEEEquad();
			break;
		default:
			assert(false && "Bad float size");							
	}
	return nullptr;
}

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
	llvm::Type* getFloatType(int size)
	{
		switch (size)
		{
			case 16:
				return llvm::Type::getHalfTy(m_llvmContext);
				break;
			case 32:
				return llvm::Type::getFloatTy(m_llvmContext);
				break;
			case 64:
				return llvm::Type::getDoubleTy(m_llvmContext);
				break;
			case 128:
				return llvm::Type::getFP128Ty(m_llvmContext);
				break;
			default:
				assert(false && "Bad float size");							
		}
		return nullptr;
	}

	Value* createIntegerConstant(uint64_t value, int size, bool isSigned)
	{
		auto iType = llvm::IntegerType::get(m_llvmContext, size);
		return llvm::ConstantInt::get(iType, value, isSigned);
	}

	Value* createIntegerConstantFromText(string text, int size, bool isSigned)
	{
		auto iType = llvm::IntegerType::get(m_llvmContext, size);
		// TODO: Make sure we don't allow stupid octal syntax
		auto val = llvm::ConstantInt::get(iType, llvm::StringRef(text), 10);
		return val;
	}

	Value* createStringConstantFromText(string text, bool nullTerminated = true)
	{
		// TODO: Should we always terminate string constants?
		/*
		auto zero = llvm::ConstantInt::get(llvm::Type::getInt8Ty(m_context), 0);
		llvm::Value* indexList[2] = {zero, zero};

		m_valueStack.push_back(m_builder.CreateGEP(
				llvm::ConstantDataArray::getString(m_context, s, true),
				indexList));*/

		//auto val = m_llvmBuilder.CreateGlobalStringPtr(text.c_str());

		auto constant = llvm::ConstantDataArray::getString(m_llvmContext, text.c_str(), nullTerminated);
		auto globData = new llvm::GlobalVariable(*m_llvmModule,
				constant->getType(),
				true,
				llvm::GlobalValue::PrivateLinkage,
				constant);
		globData->setUnnamedAddr(llvm::GlobalValue::UnnamedAddr::Global);

		return globData;
	}

	Value* createFloatConstantFromText(string text, int size)
	{
		auto val = llvm::ConstantFP::get(m_llvmContext, llvm::APFloat(*getFloatStandard(size), llvm::StringRef(text)));	
		return val;
	}

	llvm::Type* resolveType(const Type& type)
	{
		assert(type.isConcrete());
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
			else
			{
				auto size = primitive.knowsSize() ? primitive.size : DEFAULT_INT_SIZE;
				return getFloatType(size);
			}
		}

		assert("Cannot resolve type" && false);
		return nullptr;
	}

	void createAssignment(IR::Assignment& assignment)
	{
		auto* val = createValueFromExpression(*assignment.expression);
		auto* store = createPtrFromExpression(*assignment.assignable);

		m_llvmBuilder.CreateStore(val, store);
	}

	void createReturn(IR::Return& ret)
	{
		auto* val = createValueFromExpression(*ret.expr);
		m_llvmBuilder.CreateRet(val);
	}

	llvm::Value* createValueFromCall(IR::Call& call)
	{
		auto* val = createValueFromExpression(*call.callable);

		// TODO: This is not very pretty, can we strongly type callable to function?
		val = val->stripPointerCasts();
		auto func = llvm::dyn_cast<llvm::Function>(val);
		assert(func);

		std::vector<llvm::Value*> args;
		for (auto& arg : call.args)
		{
			auto* val = createValueFromExpression(*arg);
			args.push_back(val);
		}

		return m_llvmBuilder.CreateCall(func, args);
	}

	llvm::Value* createValuePtr(llvm::Value* val)
	{
    	llvm::Value *zero = llvm::ConstantInt::get(llvm::Type::getInt32Ty(m_llvmContext), 0);
    	llvm::Value *Args[] = { zero, zero };
    	return m_llvmBuilder.CreateInBoundsGEP(nullptr, val, Args);
	}

	llvm::Value* createBinaryOp(const TypeRef& type, IR::BinaryOp::OpType opType, llvm::Value* leftVal, llvm::Value* rightVal)
	{
		assert(type->isPrimitive());
		const auto& primitive = type->getPrimitive();
		if (primitive.isInteger() || primitive.isChar())
		{
			switch (opType)
			{
				case IR::BinaryOp::Eq: return m_llvmBuilder.CreateICmpEQ(leftVal, rightVal, "icmp"); 
				case IR::BinaryOp::Add: return m_llvmBuilder.CreateAdd(leftVal, rightVal, "iadd"); 
				case IR::BinaryOp::Sub: return m_llvmBuilder.CreateSub(leftVal, rightVal, "isub"); 
				case IR::BinaryOp::Mul: return m_llvmBuilder.CreateMul(leftVal, rightVal, "imul"); 
				case IR::BinaryOp::Div: return m_llvmBuilder.CreateSDiv(leftVal, rightVal, "idiv"); 
				default: assert(false);
			}
		}
		else
		{
			switch (opType)
			{
				case IR::BinaryOp::Eq: return m_llvmBuilder.CreateFCmpOEQ(leftVal, rightVal, "icmp"); 
				case IR::BinaryOp::Add: return m_llvmBuilder.CreateFAdd(leftVal, rightVal, "iadd"); 
				case IR::BinaryOp::Sub: return m_llvmBuilder.CreateFSub(leftVal, rightVal, "isub"); 
				case IR::BinaryOp::Mul: return m_llvmBuilder.CreateFMul(leftVal, rightVal, "imul"); 
				case IR::BinaryOp::Div: return m_llvmBuilder.CreateFDiv(leftVal, rightVal, "idiv"); 
				default: assert(false);
			}
		}
	}

	llvm::Value* createPtrFromExpression(IR::Expression& expr)
	{
		switch (expr.exprType)
		{
			case IR::Expression::Reference:
			{
				auto* ref = static_cast<IR::Reference*>(&expr);

				assert(ref->referenceable);
				auto& val = *ref->referenceable->value;

				// We should have generated values for all referenceables already
				assert(val.backendValue);
				const llvm::Type* valType = val.backendValue->getType();
				assert(valType->isPointerTy() && "Can only get ptr from ptr type");
				return val.backendValue;
			}
			case IR::Expression::Literal:
			case IR::Expression::Call:
			case IR::Expression::BinaryOp:
				assert(false && "Only variable pointers, for now");
		}

		assert(false);
		return nullptr;
	}

	llvm::Value* createValueFromExpression(IR::Expression& expr)
	{
		switch (expr.exprType)
		{
			case IR::Expression::Reference:
			{
				auto* ref = static_cast<IR::Reference*>(&expr);

				assert(ref->referenceable);
				auto& val = *ref->referenceable->value;

				// We should have generated values for all referenceables already
				assert(val.backendValue);

				// Hm, constants does not have storage, so use the value directly
				if (llvm::isa<llvm::Constant>(val.backendValue))	
					return val.backendValue;
				else
					return m_llvmBuilder.CreateLoad(val.backendValue);
				break;
			}
			case IR::Expression::Literal:
			{
				auto* literal = static_cast<IR::Literal*>(&expr);
				// All literals are generated at concretization
				assert(expr.backendValue);
				if (literal->isPointerType)
					return createValuePtr(expr.backendValue);
				return expr.backendValue;
				break;
			}
			case IR::Expression::Call:
			{
				auto* call = static_cast<IR::Call*>(&expr);
				return createValueFromCall(*call);
				break;
			}
			case IR::Expression::BinaryOp:
			{
				auto* binaryOp = static_cast<IR::BinaryOp*>(&expr);
				auto* leftVal = createValueFromExpression(*binaryOp->leftExpr);
				auto* rightVal = createValueFromExpression(*binaryOp->rightExpr);

				return createBinaryOp(binaryOp->getType(), binaryOp->opType, leftVal, rightVal);
				break;
			}
		}

		assert(false);
		return nullptr;
	}

	llvm::Function* createFunction(IR::Function& irfunction)
	{
		const TypeRef& funcType = irfunction.getType();
		assert(funcType->isConcrete() && "Inconcrete types are not allowed in ir generation");

		vector<string> paramNames;
		std::vector<llvm::Type*> args;

		for (const FunctionClass::Param& p : funcType->getFunction().inParams)
		{
			args.push_back(resolveType(p.type.getType()));
			paramNames.push_back(p.identifier);
		}

		// TODO: multiple return values
		llvm::Type* returnType = m_llvmBuilder.getVoidTy();
		if (!funcType->getFunction().outParams.empty())
		{
			assert(funcType->getFunction().outParams.size() == 1);
			returnType = resolveType(funcType->getFunction().outParams[0].type.getType());
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
		, m_llvmModule(llvm::make_unique<llvm::Module>("SmugModule", m_llvmContext))
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
			auto* assignment = static_cast<IR::Assignment*>(&irstatement);
			m_context.createAssignment(*assignment);
			break;
		}

		case IR::Statement::Call:
		{
			auto* call = static_cast<IR::Call*>(&irstatement);
			m_context.createValueFromCall(*call);
			break;
		}

		case IR::Statement::Conditional:
		{
			//auto* conditional = static_cast<IR::Conditional*>(&irstatement);	
			break;
		}

		case IR::Statement::Return:
		{
			auto* ret = static_cast<IR::Return*>(&irstatement);
			m_context.createReturn(*ret);
			break;
		}
		}
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

		// TODO: Generate all functions up front, so we don't have to track basic blocks?
		auto* oldBlock = m_context.m_llvmBuilder.GetInsertBlock();

		// Generate all function bodies
		for (auto* func : funcs)
		{	
			generateFunctionBody(*func);
		}

		// Restore block
		m_context.m_llvmBuilder.SetInsertPoint(oldBlock);

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

			// This parameter did not have a matching signature param in the ast, ignore it
			//	(This is currently used for main args for example)
			if (!paramPtr)
				continue;

			// TODO: Point const params to argument directly
			auto a = m_context.m_llvmBuilder.CreateAlloca(arg.getType(), nullptr, arg.getName());

			// TODO: This is not pretty, params should only be variables 
			//	(constants should not be allowed for a concretized function, right?)
			assert(paramPtr->value->valueType == IR::Value::Variable);
			m_context.m_llvmBuilder.CreateStore(&arg, a);
			paramPtr->value->backendValue = a;

			argIndex++;
		}

		generateScope(irfunction.scope);

		// HACK: Append emtpy ret on void functions to make llvm happy
		auto& functionClass = irfunction.getType()->getFunction();		
		if (functionClass.outParams.empty())
			m_context.m_llvmBuilder.CreateRetVoid();

		std::stringstream error;
		llvm::raw_os_ostream ostr(error);
		if (llvm::verifyFunction(*func, &ostr))
		{
			printLine("Error generating function: ");
			string l;
			while (getline(error, l))
			{
				printLine(l, 1);
			}
		}
	}

	void generateModule(IR::Module& irmodule)
	{
		generateFunctionHead(*irmodule.mainFunction);
		generateFunctionBody(*irmodule.mainFunction);

		std::stringstream error;
		llvm::raw_os_ostream ostr(error);
		if (llvm::verifyModule(*m_context.m_llvmModule, &ostr))
		{
			printLine("Error generating module: ");
			string l;
			while (getline(error, l))
			{
				printLine(l, 1);
			}
		}	
	}

	Generator(Context& context)
		: m_context(context)
	{
	}

	Context& m_context;
};

}