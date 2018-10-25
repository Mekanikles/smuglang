#pragma once

#include <unordered_map>

#include "llvm/ExecutionEngine/ExecutionEngine.h"
#include "llvm/ExecutionEngine/GenericValue.h"

struct Value
{
	// TODO: Should be type ref or just type?
	//	Should not be able to affect this type by inference at this point
	TypeRef type;
	vector<char> data;
};

namespace ExpressionContext
{
	static llvm::ExecutionEngine* s_engine = nullptr;
	static llvm::LLVMContext s_llvmContext;
	static llvm::IRBuilder<llvm::NoFolder> s_llvmBuilder(s_llvmContext);
	static std::unique_ptr<llvm::Module> s_llvmModule = llvm::make_unique<llvm::Module>("EvalutaionModule", s_llvmContext);
	static llvm::Module* moduleRawPtr = s_llvmModule.get();
}

struct ExpressionEvaluator : AST::Visitor
{
	ExpressionEvaluator(Context* context, Value* outValue)
		: outValue(outValue)
		, context(context)
	{
		assert(outValue);
	}

	void visit(AST::FunctionSignature* node) override
	{
		// TODO: FunctionSignature should be a type literal
		auto& val = *this->outValue;
		val.type = node->getType(this->context);

		auto length = sizeof(TypeId);
		TypeId id = val.type->typeId();
		val.data.resize(length);
		memcpy(val.data.data(), &id, length);

		this->success = true;
	}

	void visit(AST::UnaryPostfixOp* node) override
	{
		if (node->opType == TokenType::Asterisk)
		{
			// TODO: asterisk postfix op behaves as type literal
			auto& val = *this->outValue;
			val.type = node->getType(this->context);

			auto length = sizeof(TypeId);
			TypeId id = val.type->typeId();
			val.data.resize(length);
			memcpy(val.data.data(), &id, length);

			this->success = true;
		}
		else
		{
			assert(false && "Cannot evaluate postfix ops yet");
		}
	}

	void visit(AST::SymbolDeclaration* node) override
	{
		assert(node->storageQualifier == StorageQualifier::Def);
		node->initExpr->accept(this);
	}

	void visit(AST::SymbolExpression* node) override
	{
		auto* symbolDep = this->context->getSymbolDependency(node);
		auto* sourceNode = symbolDep->getSymbolSource()->getNode();
		sourceNode->accept(this);
	}

	void visit(AST::TypeLiteral* node) override
	{
		auto& val = *this->outValue;
		val.type = node->getType(this->context);

		auto length = sizeof(TypeId);
		TypeId id = val.type->typeId();
		val.data.resize(length);
		memcpy(val.data.data(), &id, length);

		this->success = true;
	}

	void visit(AST::StringLiteral* node) override
	{
		auto& val = *this->outValue;

		val.type = node->getType(this->context);

		// TODO: Handle other types of strings
		assert(isCharPointer(val.type));

		static std::unordered_map<AST::StringLiteral*, string> stringStore;
		auto it = stringStore.find(node);
		if (it == stringStore.end())
		{
			stringStore[node] = processQuotedInputString(node->value);
			it = stringStore.find(node);
		}
		string& str = it->second;
		const char* cstr = str.c_str();

		val.data.resize(sizeof(cstr));
		memcpy(val.data.data(), &cstr, sizeof(cstr));

		this->success = true;
	}

	void visit(AST::Call* node) override
	{
		using namespace ExpressionContext;

		printLine("Evaluating Call...");
		auto& val = *this->outValue;
		val.type = node->getType(this->context);

		assert(s_engine);

		// Is it necessary to remove module before modifying it?
		if (s_engine->removeModule(moduleRawPtr))
		{
			s_llvmModule = std::unique_ptr<llvm::Module>(moduleRawPtr);
		}

		std::stringstream llvmOutput;
		LLVMIRGenerator llvmGenerator(context, &llvmOutput, s_llvmModule.get(), &s_llvmContext, &s_llvmBuilder);

		node->expr->accept(&llvmGenerator);
		// TODO: SymbolExpession generates too many values
		assert(llvmGenerator.m_valueStack.size() == 1);
		llvm::Value* llvmValue = llvmGenerator.m_valueStack.back();

		// TODO: should probably not allow pointer types for compile-time generated code
		//	These will point into compiler memory and cannot be treated as constants without
		//	some kind of transformation first.
		llvmValue = llvmValue->stripPointerCasts();

		// TODO: How to get inner type of pointer value
		//assert(llvmValue->getType()->getTypeID() == llvm::Type::FunctionTyID);
		auto llvmFunc = static_cast<llvm::Function*>(llvmValue);

		llvm::verifyFunction(*llvmFunc);
		llvm::verifyModule(*s_llvmModule);
		llvm::legacy::PassManager passManager;
		std::stringstream llvmOutput2;
		llvm::raw_os_ostream llvmOut(llvmOutput2);
		passManager.add(llvm::createPrintModulePass(llvmOut));
		passManager.run(*s_llvmModule);		

		printLine("Generated Evaluation IR:");
		string l;
		while (getline(llvmOutput2, l))
		{
			printLine(l, 1);
		}

		printLine("Evaluating IR...");
		// Re-add module after modifying
		s_engine->addModule(std::move(s_llvmModule));

 		vector<llvm::GenericValue> argValues;
		llvm::GenericValue genericVal = s_engine->runFunction(llvmFunc, argValues);

		const int sizeBytes = val.type.getSize();
		val.data.resize(sizeBytes + 4);
		// Add sentinel at the end to ensure no overflows
		*((uint*)&val.data[sizeBytes]) = 0xDEADBEEF;

		s_engine->StoreValueToMemory(genericVal, (llvm::GenericValue*)&val.data[0], llvmFunc->getReturnType());

		assert(*((uint*)&val.data[sizeBytes]) == 0xDEADBEEF && "Copied value exceeded known type size");

		printLine("Evaluation data result:");
		const uint num64bitWords = (sizeBytes >> 3);
		for (uint i = 0; i < num64bitWords; ++i)
		{
			auto data = (uint64_t*)val.data.data();
			char num[256];
			snprintf(num, 256, "0x%016llx", data[i]);
			printLine(num, 1);
		}

		this->success = true;
	}

	bool success = false;
	Value* outValue = nullptr;
	Context* context;	
};

bool initExpressionEvaluator()
{
	using namespace ExpressionContext;
	assert(s_engine == nullptr);

	// Init llvm stuff
	{
		llvm::InitializeAllTargetInfos();
		llvm::InitializeAllTargets();
		llvm::InitializeAllTargetMCs();
		llvm::InitializeAllAsmParsers();
		llvm::InitializeAllAsmPrinters();
		LLVMLinkInInterpreter();
	}

	// Set up target machine and configure module 
	{
		auto targetTriple = llvm::sys::getDefaultTargetTriple();
		s_llvmModule->setTargetTriple(targetTriple);

		std::string errorString;
		auto target = llvm::TargetRegistry::lookupTarget(targetTriple, errorString);

		// Print an error and exit if we couldn't find the requested target.
		// This generally occurs if we've forgotten to initialise the
		// TargetRegistry or we have a bogus target triple.
		if (!target) 
		{
			llvm::errs() << "Target not found";
			return false;
		}

		auto targetCPU = "generic";
		auto targetFeatures = "";

		llvm::TargetOptions targetOptions;
		auto targetModel = llvm::Reloc::Model();
		auto targetMachine = target->createTargetMachine(targetTriple, 
				targetCPU, targetFeatures, targetOptions, targetModel);

		s_llvmModule->setDataLayout(targetMachine->createDataLayout());
	}

	llvm::EngineBuilder engineBuilder(std::move(s_llvmModule));
	engineBuilder.setEngineKind(llvm::EngineKind::Interpreter);

	printLine("Creating evalutor engine");
	string errorStr;
	engineBuilder.setErrorStr(&errorStr);
	s_engine = engineBuilder.create();
	if (!s_engine)
	{
		printLine(errorStr, 1);
		return false;
	}

	return true;
}

bool evaluateExpression(Context* context, AST::Expression* expr, Value* outValue)
{
	ExpressionEvaluator e(context, outValue);
	expr->accept(&e);

	return e.success;
}