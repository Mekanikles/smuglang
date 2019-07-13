#pragma once

#include "backend/context.h"
#include "ir.h"

namespace Backend
{

struct ScopeInfo
{
	ScopeInfo* parent = nullptr;
	IR::Scope* irScope = nullptr;
	llvm::BasicBlock* loopContinueBlock = nullptr;
	llvm::BasicBlock* loopBreakBlock = nullptr;
};

struct FunctionGenerator
{
	void generateStatement(IR::Statement& irstatement)
	{
		switch (irstatement.statementType)
		{
		case IR::Statement::Scope:
		{
			auto* scope = static_cast<IR::Scope*>(&irstatement);

			ScopeInfo scopeInfo;
			scopeInfo.parent = &currentScope();
			scopeInfo.irScope = scope;
			scopeInfo.loopContinueBlock = scopeInfo.parent->loopContinueBlock;
			scopeInfo.loopBreakBlock = scopeInfo.parent->loopBreakBlock;
			generateScope(scopeInfo);

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
			bool hasFalseBranch = !cond->falseScope.isEmpty();
			
			auto* trueBlock = llvm::BasicBlock::Create(m_context.m_llvmContext, "true");
			auto* contBlock = llvm::BasicBlock::Create(m_context.m_llvmContext, "cont");
			auto* falseBlock = hasFalseBranch ? llvm::BasicBlock::Create(m_context.m_llvmContext, "false") : contBlock;

			auto* val = m_context.createValueFromExpression(*cond->expr);
			m_context.m_llvmBuilder.CreateCondBr(val, trueBlock, falseBlock);

			auto func = m_context.m_llvmBuilder.GetInsertBlock()->getParent();

			// True branch
			func->getBasicBlockList().push_back(trueBlock);
			m_context.m_llvmBuilder.SetInsertPoint(trueBlock);

			ScopeInfo scopeInfo;
			scopeInfo.parent = &currentScope();
			scopeInfo.irScope = &cond->trueScope;
			scopeInfo.loopContinueBlock = scopeInfo.parent->loopContinueBlock;
			scopeInfo.loopBreakBlock = scopeInfo.parent->loopBreakBlock;			
			generateScope(scopeInfo);

			m_context.m_llvmBuilder.CreateBr(contBlock);

			// False branch
			if (hasFalseBranch)
			{
				func->getBasicBlockList().push_back(falseBlock);
				m_context.m_llvmBuilder.SetInsertPoint(falseBlock);

				ScopeInfo scopeInfo;
				scopeInfo.parent = &currentScope();
				scopeInfo.irScope = &cond->falseScope;
				scopeInfo.loopContinueBlock = scopeInfo.parent->loopContinueBlock;
				scopeInfo.loopBreakBlock = scopeInfo.parent->loopBreakBlock;			
				generateScope(scopeInfo);

				m_context.m_llvmBuilder.CreateBr(contBlock);
			}

			// TODO: Phi nodes and whatnot
			// Continue block
			func->getBasicBlockList().push_back(contBlock);
			m_context.m_llvmBuilder.SetInsertPoint(contBlock);

			break;
		}

		case IR::Statement::Loop:
		{
			auto* irLoop = static_cast<IR::Loop*>(&irstatement);
			auto* loopBlock = llvm::BasicBlock::Create(m_context.m_llvmContext, "loop_entry");
			auto* nextBlock = llvm::BasicBlock::Create(m_context.m_llvmContext, "loop_next");

			auto parentBlock = m_context.m_llvmBuilder.GetInsertBlock()->getParent();

			// Enter loop block
			m_context.m_llvmBuilder.CreateBr(loopBlock);

			// Loop block
			{
				parentBlock->getBasicBlockList().push_back(loopBlock);
				m_context.m_llvmBuilder.SetInsertPoint(loopBlock);

				ScopeInfo scopeInfo;
				scopeInfo.parent = &currentScope();
				scopeInfo.irScope = &irLoop->scope;
				scopeInfo.loopContinueBlock = loopBlock;
				scopeInfo.loopBreakBlock = nextBlock;
				generateScope(scopeInfo);

				m_context.m_llvmBuilder.CreateBr(loopBlock);
			}

			// Next block
			parentBlock->getBasicBlockList().push_back(nextBlock);
			m_context.m_llvmBuilder.SetInsertPoint(nextBlock);			

			break;
		}

		case IR::Statement::Continue:
		{
			assert(currentScope().loopContinueBlock);

			// Jump to loop entry
			m_context.m_llvmBuilder.CreateBr(currentScope().loopContinueBlock);

			// Need to add an dummy block here since the branch instruction is a terminator
			m_context.createDummyBlock();
			break;
		}

		case IR::Statement::Break:
		{
			assert(currentScope().loopBreakBlock);

			// Jump to loop exit
			m_context.m_llvmBuilder.CreateBr(currentScope().loopBreakBlock);

			// Need to add an dummy block here since the branch instruction is a terminator
			//	in case there are more statements after the break
			m_context.createDummyBlock();
			break;
		}				

		case IR::Statement::Return:
		{
			auto* ret = static_cast<IR::Return*>(&irstatement);

			// Since returns can need cleanup, we just record the point of return
			//	so we can add the correct instruction later
			//currentScope().addReturnPoint(m_context.m_llvmBuilder.GetInsertBlock(), val);

			// First resolve return value, in case finalizers affect it
			auto* val = ret->expr ? m_context.createValueFromExpression(*(ret->expr)) : nullptr;

			// Finalizers
			{
				ScopeInfo* scope = &currentScope();
				int scopeLocalIndex = ret->scopeLocalIndex;
				auto* currentBlock = m_context.m_llvmBuilder.GetInsertBlock();
				auto* finalizerBlock = llvm::BasicBlock::Create(m_context.m_llvmContext, scope->irScope->getScopeBlockName("_finalizer"));
				m_context.m_llvmBuilder.SetInsertPoint(finalizerBlock);

				while (scope)
				{
					IR::Scope* irScope = scope->irScope;
					// Generate defer statements in reverse order
					for (auto it = irScope->deferStatements.rbegin(); it != irScope->deferStatements.rend(); ++it)
					{
						IR::Defer* deferStatement = *it;
						// Only generate finalizers for defers that are before us, chronologically
						if (deferStatement->scopeLocalIndex <= scopeLocalIndex)
						{
							ScopeInfo scopeInfo;
							scopeInfo.parent = &currentScope();
							scopeInfo.irScope = &deferStatement->scope;
							generateScope(scopeInfo);
						}
					}

					scopeLocalIndex = scope->irScope->scopeLocalIndex;
					scope = scope->parent;
				}

				if (!finalizerBlock->empty())
				{
					auto parentBlock = currentBlock->getParent();
					parentBlock->getBasicBlockList().push_back(finalizerBlock);
					m_context.m_llvmBuilder.SetInsertPoint(currentBlock);
					m_context.m_llvmBuilder.CreateBr(finalizerBlock);
					m_context.m_llvmBuilder.SetInsertPoint(finalizerBlock);
				}
				else
				{
					m_context.m_llvmBuilder.SetInsertPoint(currentBlock);
					delete finalizerBlock;
				}
			}

			if (val)
				m_context.m_llvmBuilder.CreateRet(val);
			else
				m_context.m_llvmBuilder.CreateRetVoid();

			// Need to add an dummy block here so that the later instruction injection
			//	happens at the end of the current block
			m_context.createDummyBlock();			
			break;
		}

		case IR::Statement::Defer:
		{
			// Defers happen on returns/exiting scopes
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

	void generateScope(ScopeInfo& scopeInfo)
	{
		assert(scopeInfo.irScope);
		IR::Scope& irscope = *scopeInfo.irScope;

		m_scopeStack.push_back(&scopeInfo);

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

		m_scopeStack.pop_back();

		// Finalizers
		{
			auto* currentBlock = m_context.m_llvmBuilder.GetInsertBlock();
			auto* finalizerBlock = llvm::BasicBlock::Create(m_context.m_llvmContext, irscope.getScopeBlockName("_finalizer"));
			m_context.m_llvmBuilder.SetInsertPoint(finalizerBlock);

			// Generate defer statements in reverse order
			for (auto it = irscope.deferStatements.rbegin(); it != irscope.deferStatements.rend(); ++it)
			{
				IR::Defer* deferStatement = *it;

				ScopeInfo scopeInfo;
				scopeInfo.parent = &scopeInfo;
				scopeInfo.irScope = &deferStatement->scope;
				generateScope(scopeInfo);
			}
			
			if (!finalizerBlock->empty())
			{
				auto parentBlock = currentBlock->getParent();
				parentBlock->getBasicBlockList().push_back(finalizerBlock);
				m_context.m_llvmBuilder.SetInsertPoint(currentBlock);
				m_context.m_llvmBuilder.CreateBr(finalizerBlock);
				m_context.m_llvmBuilder.SetInsertPoint(finalizerBlock);
			}
			else
			{
				m_context.m_llvmBuilder.SetInsertPoint(currentBlock);
				delete finalizerBlock;
			}
		}

	}

	void generateFunction(IR::Function& irfunction)
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

		ScopeInfo scopeInfo;
		scopeInfo.irScope = &irfunction.getScope();
		generateScope(scopeInfo);

		// TODO: Remove since scopes are now handling this?
		// HACK: Append empty ret on void functions to make llvm happy
		auto& functionClass = irfunction.getType()->getFunction();		
		if (functionClass.outParams.empty())
			m_context.m_llvmBuilder.CreateRetVoid();
		else
		{
			// If we cannot return void, we need another terminator, since
			//	all return statements also create dummy followup blocks
			// TODO: Catch this in IR stage and don't generate unreachable code
			m_context.m_llvmBuilder.CreateUnreachable();
		}

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

	ScopeInfo& currentScope() { assert(!m_scopeStack.empty()); return *m_scopeStack.back(); }

	FunctionGenerator(Context& context)
		: m_context(context)
	{
	}

	Context& m_context;
	vector<ScopeInfo*> m_scopeStack;
};

struct Generator
{
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
		FunctionGenerator generator(m_context);
		generator.generateFunction(irfunction);
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

	void generateLocalMain(IR::Module& irmodule)
	{
		generateFunctionHead(*irmodule.localMain, "__localmain");
		generateFunctionBody(*irmodule.localMain);
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
		generateLocalMain(irmodule);

		// External entry point
		{
			vector<string> paramNames { "argc", "argv" };
			std::vector<llvm::Type*> args {
				llvm::IntegerType::get(m_context.m_llvmContext, 32), 
				llvm::IntegerType::get(m_context.m_llvmContext, 8)->getPointerTo() };

			llvm::Type* returnType = llvm::IntegerType::get(m_context.m_llvmContext, 32);

			auto functionType = llvm::FunctionType::get(
					returnType, args, false);

			auto func = llvm::Function::Create(functionType, llvm::Function::ExternalLinkage, "main", &*m_context.m_llvmModule);

			// Assign parameter names
			uint i = 0;
			for (auto& a : func->args())
				a.setName(paramNames[i++]);

			auto entryBlock = llvm::BasicBlock::Create(m_context.m_llvmContext, "entry", func);
			m_context.m_llvmBuilder.SetInsertPoint(entryBlock);

			// Call localmain
			m_context.m_llvmBuilder.CreateCall(irmodule.localMain->backendValue);
			
			// Return 0
			llvm::Value* zero = llvm::ConstantInt::get(llvm::Type::getInt32Ty(m_context.m_llvmContext), 0);
			m_context.m_llvmBuilder.CreateRet(zero);
		}

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

} // namespace
