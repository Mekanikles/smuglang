#pragma once
#include <unordered_map>

#include "core.h"
#include "value.h"
#include "ir.h"
#include "types.h"

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

// HACK: We need the ir module when generating expressions (for function literals)
//	can we get rid of this dependency? Otherwise, add the IR module to the generator context
static IR::Module* s_hackCurrentIRModule = nullptr;

const uint DEFAULT_INT_SIZE = 32;
const bool DEFAULT_INT_ISSIGNED = true;

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

	Value* createFloatConstant(double d, int size)
	{
		return llvm::ConstantFP::get(getFloatType(size), d);
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
		else if (type.isStruct())
		{
			auto* llvmStruct = this->m_structTypeMap[type.typeId()];
			if (!llvmStruct)
			{
				const auto& structType = type.getStruct();

				vector<llvm::Type*> llvmMembers;
				for (const StructClass::Field& field : structType.fields)
				{
					auto llvmType = resolveType(field.type.getType());
					llvmMembers.push_back(llvmType);
				}

				llvmStruct = llvm::StructType::create(m_llvmContext, structType.name);
				llvmStruct->setBody(llvmMembers);
				this->m_structTypeMap[type.typeId()] = llvmStruct;
			}
			return llvmStruct;
		}

		assert("Cannot resolve type" && false);
		return nullptr;
	}

	void createAndSetBlock(string name)
	{
		auto block = llvm::BasicBlock::Create(m_llvmContext, name.c_str());
		m_llvmBuilder.SetInsertPoint(block);

		// TODO: Dangling block. Need to expose blocks in backend api?
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

		const bool isVariadic = func->isVarArg();
		const int funcArgCount = func->arg_size();

		std::vector<llvm::Value*> args;
		int argCount = 0;
		for (auto& arg : call.args)
		{
			argCount++;
			auto* val = createValueFromExpression(*arg);
			if (argCount > funcArgCount)
			{
				assert(isVariadic);
				// C ABI: Varags cannot handle floats < 64bit, add a cast to double
				if (val->getType()->isFloatTy() || val->getType()->isHalfTy())
				{
					val = m_llvmBuilder.CreateFPCast(val, llvm::Type::getDoubleTy(m_llvmContext), "vararg_cast");
				}
			}

			args.push_back(val);
		}

		return m_llvmBuilder.CreateCall(func, args);
	}

	llvm::Value* createValuePtr(llvm::Value* val)
	{
    	llvm::Value* zero = llvm::ConstantInt::get(llvm::Type::getInt32Ty(m_llvmContext), 0);
    	llvm::Value* args[] = { zero, zero };
    	return m_llvmBuilder.CreateInBoundsGEP(nullptr, val, args);
	}

	// Use "Floored Division" modulo algorithm, to avoid common pitfalls
	//	with using modulo for keeping indices in range
	llvm::Value* createIntegerModulo(llvm::Value* leftVal, llvm::Value* rightVal)
	{
		auto* rem = m_llvmBuilder.CreateSRem(leftVal, rightVal, "srem");
		return rem;
	}

	// Natively support float modulo, use same definition as integer modulo
	llvm::Value* createFloatModulo(llvm::Value* leftVal, llvm::Value* rightVal)
	{

		assert(false);
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
				case IR::BinaryOp::Mod: return createIntegerModulo(leftVal, rightVal); 
				case IR::BinaryOp::LT: return m_llvmBuilder.CreateICmpSLT(leftVal, rightVal, "ilt"); 
				case IR::BinaryOp::GT: return m_llvmBuilder.CreateICmpSGT(leftVal, rightVal, "igt"); 
				case IR::BinaryOp::LTE: return m_llvmBuilder.CreateICmpSLE(leftVal, rightVal, "ilte"); 
				case IR::BinaryOp::GTE: return m_llvmBuilder.CreateICmpSGE(leftVal, rightVal, "igte"); 
				
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
				case IR::BinaryOp::Mod: return createFloatModulo(leftVal, rightVal); 
				case IR::BinaryOp::LT: return m_llvmBuilder.CreateFCmpOLT(leftVal, rightVal, "ilt"); 
				case IR::BinaryOp::GT: return m_llvmBuilder.CreateFCmpOGT(leftVal, rightVal, "igt"); 
				case IR::BinaryOp::LTE: return m_llvmBuilder.CreateFCmpOLE(leftVal, rightVal, "ilte"); 
				case IR::BinaryOp::GTE: return m_llvmBuilder.CreateFCmpOGE(leftVal, rightVal, "igte"); 

				default: assert(false);
			}
		}
	}

	llvm::Value* createUnaryOp(const TypeRef& type, IR::UnaryOp::OpType opType, llvm::Value* val)
	{
		assert(type->isPrimitive());
		const auto& primitive = type->getPrimitive();
		if (primitive.isInteger() || primitive.isChar())
		{
			switch (opType)
			{
				case IR::UnaryOp::Neg: return m_llvmBuilder.CreateNeg(val, "ineg");
				default: assert(false);
			}
		}
		else
		{
			switch (opType)
			{
				case IR::UnaryOp::Neg: return m_llvmBuilder.CreateFNeg(val, "fneg");
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
				auto& val = *ref->referenceable;

				// We should have generated values for all referenceables already
				assert(val.backendValue);
				const llvm::Type* valType = val.backendValue->getType();
				assert(valType->isPointerTy() && "Can only get ptr from ptr type");
				return val.backendValue;
			}	
			case IR::Expression::MemberAccess:
			{
				auto* maccess = static_cast<IR::MemberAccess*>(&expr);
				auto* leftSidePtr = createPtrFromExpression(*maccess->expression);

				const TypeRef& type = maccess->expression->getType();
				int index = type->getStruct().getFieldIndexByName(maccess->name);

				llvm::Value* zero = llvm::ConstantInt::get(llvm::Type::getInt32Ty(m_llvmContext), 0);
				llvm::Value* indexVal = llvm::ConstantInt::get(llvm::Type::getInt32Ty(m_llvmContext), index);
				llvm::Value* gepIndices[] = { zero, indexVal };

				auto* val = m_llvmBuilder.CreateGEP(leftSidePtr, gepIndices, maccess->name);
				return val;
				break;
			}	
			default:
				assert(false && "Only variable pointers, for now");
		}

		assert(false);
		return nullptr;
	}

	llvm::Value* createValueFromTypeAndDataBlock(const TypeRef& type, vector<u8>& data)
	{
		if (type->isPointer())
		{
			auto& innerType = type->getPointer().type;
			auto* val = createValueFromTypeAndDataBlock(innerType, data);
			return createValuePtr(val);
		}
		else
		{
			const uint8_t* rawdata = reinterpret_cast<const uint8_t*>(data.data());
			auto constant = llvm::ConstantDataArray::get(m_llvmContext, llvm::makeArrayRef(rawdata, data.size()));
			auto globData = new llvm::GlobalVariable(*m_llvmModule,
				constant->getType(),
				true,
				llvm::GlobalValue::PrivateLinkage,
				constant);
			globData->setUnnamedAddr(llvm::GlobalValue::UnnamedAddr::Global);
			return globData;
		}
	}

	llvm::Value* createValueFromLiteral(IR::Literal& literal)
	{
		auto& type = literal.getType();
		assert(type->isConcrete());
		assert(!literal.backendValue);

		if (type->isPrimitive())
		{
			auto& p = type->getPrimitive();
			if (p.isInteger())
			{
				long long l = literal.readValue<long long>();
				literal.backendValue = createIntegerConstant((uint64_t)l, p.size, p.isSigned());
			}
			else if (p.isFloat())
			{
				double d = literal.readValue<double>();
				literal.backendValue = createFloatConstant(d, p.size);
			}
			else
			{
				assert(false && "Unsupported primitive");
			}
		}
		else if (type->isFunction())
		{
			// Here we cheat a bit, the literal is the function id, but we want to treat that value
			//	as the function ptr in generated code
			const IR::FunctionId id  = literal.readValue<IR::FunctionId>();

			IR::Function* func = s_hackCurrentIRModule->getFunction(id);
			assert(func);
			assert(func->backendValue);

			literal.backendValue = func->backendValue;
		}
		else
		{
			literal.backendValue = createValueFromTypeAndDataBlock(literal.getType(), literal.data);
		}

		return literal.backendValue;
	}

	llvm::Value* createValueFromExpression(IR::Expression& expr)
	{
		switch (expr.exprType)
		{
			case IR::Expression::Reference:
			{
				auto* ref = static_cast<IR::Reference*>(&expr);

				assert(ref->referenceable);
				auto& val = *ref->referenceable;

				// We should have generated values for all referenceables already
				assert(val.backendValue);

				// Hm, constants does not have storage, so use the value directly
				if (llvm::isa<llvm::Constant>(val.backendValue))	
					return val.backendValue;
				else
					return m_llvmBuilder.CreateLoad(val.backendValue);
				break;
			}
			case IR::Expression::MemberAccess:
			{
				auto* maccess = static_cast<IR::MemberAccess*>(&expr);
				auto* leftSidePtr = createPtrFromExpression(*maccess->expression);

				const TypeRef& type = maccess->expression->getType();
				int index = type->getStruct().getFieldIndexByName(maccess->name);

				llvm::Value* zero = llvm::ConstantInt::get(llvm::Type::getInt32Ty(m_llvmContext), 0);
				llvm::Value* indexVal = llvm::ConstantInt::get(llvm::Type::getInt32Ty(m_llvmContext), index);
				llvm::Value* gepIndices[] = { zero, indexVal };

				auto* val = m_llvmBuilder.CreateGEP(leftSidePtr, gepIndices, maccess->name);
				return m_llvmBuilder.CreateLoad(val);
				break;
			}	
			case IR::Expression::Literal:
			{
				auto* literal = static_cast<IR::Literal*>(&expr);
				return createValueFromLiteral(*literal);
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
			case IR::Expression::UnaryOp:
			{
				auto* unaryOp = static_cast<IR::UnaryOp*>(&expr);
				auto* val = createValueFromExpression(*unaryOp->expr);

				return createUnaryOp(unaryOp->getType(), unaryOp->opType, val);
				break;
			}		
		}

		assert(false);
		return nullptr;
	}

	llvm::Function* createFunction(const FunctionClass& function, string name)
	{
		vector<string> paramNames;
		std::vector<llvm::Type*> args;

		for (const FunctionClass::Param& p : function.inParams)
		{
			args.push_back(resolveType(p.type.getType()));
			paramNames.push_back(p.identifier);
		}

		// TODO: multiple return values
		llvm::Type* returnType = m_llvmBuilder.getVoidTy();
		if (!function.outParams.empty())
		{
			assert(function.outParams.size() == 1);
			returnType = resolveType(function.outParams[0].type.getType());
		}

		auto functionType = llvm::FunctionType::get(
				returnType, args, function.isCVariadic);

		auto func = llvm::Function::Create(functionType, llvm::Function::ExternalLinkage, name, &*m_llvmModule);

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

	std::unordered_map<TypeId, llvm::StructType*> m_structTypeMap;
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
			auto* cond = static_cast<IR::Conditional*>(&irstatement);
			bool hasFalseBranch = !cond->falseBlock.isEmpty();
			
			auto* trueBlock = llvm::BasicBlock::Create(m_context.m_llvmContext, "true");
			auto* contBlock = llvm::BasicBlock::Create(m_context.m_llvmContext, "cont");
			auto* falseBlock = hasFalseBranch ? llvm::BasicBlock::Create(m_context.m_llvmContext, "false") : contBlock;

			auto* val = m_context.createValueFromExpression(*cond->expr);
			m_context.m_llvmBuilder.CreateCondBr(val, trueBlock, falseBlock);

			auto func = m_context.m_llvmBuilder.GetInsertBlock()->getParent();

			// True branch
			func->getBasicBlockList().push_back(trueBlock);
			m_context.m_llvmBuilder.SetInsertPoint(trueBlock);
			generateBlock(cond->trueBlock);
			m_context.m_llvmBuilder.CreateBr(contBlock);

			// False branch
			if (hasFalseBranch)
			{
				func->getBasicBlockList().push_back(falseBlock);
				m_context.m_llvmBuilder.SetInsertPoint(falseBlock);
				generateBlock(cond->falseBlock);
				m_context.m_llvmBuilder.CreateBr(contBlock);
			}

			// TODO: Phi nodes and whatnot
			// Continue block
			func->getBasicBlockList().push_back(contBlock);
			m_context.m_llvmBuilder.SetInsertPoint(contBlock);

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
		for (auto& var : irscope.variables)
		{	
			auto* type = m_context.resolveType(var->getType());
			assert(!var->backendValue);
			var->backendValue = m_context.m_llvmBuilder.CreateAlloca(type, nullptr, var->getName());
		}

		// Generate all blocks
		for (auto& block : irscope.blocks)
		{
			generateBlock(*block);
		}
	}

	void generateFunctionHead(IR::Function& irfunction, string name)
	{
		const TypeRef& funcType = irfunction.getType();
		assert(funcType->isConcrete() && "Inconcrete types are not allowed in ir generation");
		assert(funcType->isFunction());

		auto* func = m_context.createFunction(funcType->getFunction(), name);
		assert(!irfunction.backendValue);
		irfunction.backendValue = func;
	}

	void generateFunctionBody(IR::Function& irfunction)
	{
		assert(irfunction.backendValue);
		auto* func = static_cast<llvm::Function*>(irfunction.backendValue);

		auto entryBlock = llvm::BasicBlock::Create(m_context.m_llvmContext, "entry", func);
		m_context.m_llvmBuilder.SetInsertPoint(entryBlock);

		// Link signature variables to function argument value
		auto& signature = irfunction.getSignature();
		uint argIndex = 0;
		for (auto& arg : func->args())
		{
			// Ugh, search for param with this index
			IR::Param* paramPtr = nullptr;
			for (auto& p : signature.inParams)
			{
				IR::Param& param = *p;
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

			m_context.m_llvmBuilder.CreateStore(&arg, a);
			paramPtr->backendValue = a;

			argIndex++;
		}

		generateScope(irfunction.getScope());

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
	void generateConstant(IR::Constant& constant)
	{
		assert(!constant.backendValue);
		constant.backendValue = m_context.createValueFromLiteral(*constant.literal);
	}

	void generateExternal(IR::External& external)
	{
		assert(external.getType()->isFunction() && "Only supports external functions for now");
		
		if (!external.linkable->backendValue)
			external.linkable->backendValue = m_context.createFunction(external.getType()->getFunction(), external.getName());
		assert(!external.backendValue);
		external.backendValue = external.linkable->backendValue;
	}

	void generateMain(IR::Module& irmodule)
	{
		generateFunctionHead(*irmodule.main, "main");
		generateFunctionBody(*irmodule.main);
	}

	void generateModule(IR::Module& irmodule)
	{
		s_hackCurrentIRModule = &irmodule;

		vector<IR::Function*> funcs;

		// Take care of external symbols
		for (auto& external : irmodule.externals)
		{
			generateExternal(*external);
		}

		// Generate all function heads so we can fold constants to the underlying backend value
		for (auto& function : irmodule.functions)
		{
			// TODO: Generalize literal generation to include functions
			auto& func = *function;
			generateFunctionHead(func, func.getName());
			assert(func.backendValue);
			funcs.push_back(&func);
		}

		// Generate all constants before function bodies, so we can cross reference between functions
		for (auto& constant : irmodule.constants)
		{
			generateConstant(*constant);
		}

		// Generate all function bodies
		for (auto* func : funcs)
		{	
			generateFunctionBody(*func);
		}

		// Generate all constants

		// No-one can reference main, so generate it last
		generateMain(irmodule);

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

		s_hackCurrentIRModule = nullptr;
	}

	Generator(Context& context)
		: m_context(context)
	{
	}

	Context& m_context;
};

}