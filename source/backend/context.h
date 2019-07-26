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

#include "core.h"
#include "types.h"
#include "ir.h"
#include "backend/value.h"

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

	void createReturn(IR::Expression& expr)
	{
		auto* val = createValueFromExpression(expr);
		m_llvmBuilder.CreateRet(val);
	}

	void createDummyBlock()
	{
		auto* dummyBlock = llvm::BasicBlock::Create(m_llvmContext, "dummy");
		auto parentBlock = m_llvmBuilder.GetInsertBlock()->getParent();
		parentBlock->getBasicBlockList().push_back(dummyBlock);
		m_llvmBuilder.SetInsertPoint(dummyBlock);
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
	llvm::Value* createIntegerModulo(llvm::Value* xVal, llvm::Value* yVal)
	{
    	llvm::Value* zero = llvm::ConstantInt::get(llvm::Type::getInt32Ty(m_llvmContext), 0);


		// Base modulo on srem adjusted for 2 case where sgn(x) != sgn(y)
		// llvm optimized code, %0 is x, %1 is y, %9 is result:
		//	%3 = srem i32 %0, %1
		//	%4 = icmp ne i32 %3, 0
		//	%5 = mul nsw i32 %1, %0
		//	%6 = icmp slt i32 %5, 0
		//	%7 = and i1 %6, %4
		//	%8 = select i1 %7, i32 %1, i32 0
		//	%9 = add nsw i32 %8, %3
		auto* val3 = m_llvmBuilder.CreateSRem(xVal, yVal, "imod_srem");
		auto* val4 = m_llvmBuilder.CreateICmpNE(val3, zero, "imod_ine");
		auto* val5 = m_llvmBuilder.CreateNSWMul(yVal, xVal, "imod_mul");
		auto* val6 = m_llvmBuilder.CreateICmpSLT(val5, zero, "imod_ilt");
		auto* val7 = m_llvmBuilder.CreateAnd(val6, val4, "imod_and");
		auto* val8 = m_llvmBuilder.CreateSelect(val7, yVal, zero, "imod_sel");
		auto* val13 = m_llvmBuilder.CreateNSWAdd(val8, val3, "imod_add");

		return val13;
	}

	// Natively support float modulo, use same definition as integer modulo
	llvm::Value* createFloatModulo(llvm::Value* leftVal, llvm::Value* rightVal)
	{
		assert(false && "Float modulo not yet implemented!");
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
		if (literal.backendValue)
			return literal.backendValue;

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

				// Hm, constants and externals does not have storage, so use the value directly
				// TODO: This likely does not apply to external non-functions, fix
				if (val.isConstant() || val.isExternal())
				{
					return val.backendValue;
				}
				else
				{
					return m_llvmBuilder.CreateLoad(val.backendValue);
				}
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

} // namespace backend