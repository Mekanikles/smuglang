#pragma once
#include "core.h"

#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/LLVMContext.h"

using BackendValue = llvm::Value;

struct Backend
{
	llvm::LLVMContext llvmContext;

	BackendValue* createIntegerValueFromText(string text, int size, bool isSigned)
	{
		auto iType = llvm::IntegerType::get(this->llvmContext, size);
		// TODO: Make sure we don't allow stupid octal syntax
		auto val = llvm::ConstantInt::get(iType, llvm::StringRef(text), 10);
		return val;
	}

	Backend()
	{
	}
};