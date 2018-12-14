#pragma once
#include "core.h"
#include "ir.h"
#include "ast.h"
#include "context.h"
#include "backend/backend.h"

template<typename T, typename... Args>
T* createStatement(Args... args)
{
	T* n = createObject<T>(std::forward<Args>(args)...);
	return n;
}

template<typename T, typename... Args>
std::unique_ptr<T> createExpression(Args... args)
{
	T* n = createObject<T>(std::forward<Args>(args)...);
	return std::unique_ptr<T>(n);
}

struct ConcretizerContext
{
	Backend::Context* backend;
	IR::Module* module;
};

struct ExpressionConcretizer : AST::Visitor
{
	virtual void visit(AST::SymbolExpression* node) 
	{
		SymbolDependency* symDep = this->astContext->getSymbolDependency(node);
		auto* source = symDep->source;

		IR::Referenceable* ref = this->context->module->getReferenceable(source);
		assert(ref);

		expressionStack.push_back(createExpression<IR::Reference>(ref));
	}

	virtual void visit(AST::IntegerLiteral* node) 
	{
		TypeRef& type = node->getType(this->astContext);
		// TODO: Does support for default types go here?
		assert(type.getType().isConcrete());
		assert(type.getType().isPrimitive());
		const PrimitiveClass& primitive = type.getType().getPrimitive();
		assert(primitive.primitiveType == PrimitiveClass::Int);

		// Might as well create the value here directly
		auto* value = this->context->backend->createIntegerConstantFromText(node->value, primitive.size, primitive.signedType == PrimitiveClass::Signed);
		expressionStack.push_back(createExpression<IR::Literal>(type, value));
	}

	virtual void visit(AST::StringLiteral* node) 
	{
		TypeRef& type = node->getType(this->astContext);
		// TODO: Does support for default types go here?
		assert(type->isConcrete());
		assert(isStringType(type));

		// Might as well create the value here directly
		const string str = processQuotedInputString(node->value);
		const bool isCString = type->isPointer();

		auto* value = this->context->backend->createStringConstantFromText(str);
		expressionStack.push_back(createExpression<IR::Literal>(type, value, isCString));
	}

	ExpressionConcretizer(ConcretizerContext* context, Context* astContext, IR::Function* function, IR::Block* block)
		: context(context)
		, function(function)
		, astContext(astContext)
	{
		this->currentBlock = block;
	}

	ConcretizerContext* context;
	IR::Function* function;
	IR::Block* currentBlock;
	Context* astContext;
	vector<std::unique_ptr<IR::Expression>> expressionStack;
};

// TODO: StatementConcretizer?
struct FunctionConcretizer : AST::Visitor
{
	virtual void visit(AST::FunctionDeclaration* node) 
	{
		auto* ss = astContext->getSymbolSource(node);
		assert(ss);
		auto* ref = this->context->module->getReferenceable(ss);
		// TODO: :(
		assert(ref);
		auto* func = static_cast<IR::Function*>(ref->value.get());
		assert(func);
		generateConcreteFunction(*func, node->funcLiteral);
	}

	virtual void visit(AST::StatementBody* node) 
	{
		auto* scope = this->currentBlock->addStatement(std::make_unique<IR::Scope>());
		
		generateConcreteStatementBody(scope, node);
	}

	virtual void visit(AST::Call* node) 
	{
		IR::Call* call = new IR::Call(node->getType(this->astContext));

		auto exprs = generateConcreteExpression(node->expr);
		assert(exprs.size() == 1);
		call->setCallable(std::move(exprs.back()));

		for (auto* arg : node->args)
		{
			auto exprs = generateConcreteExpression(arg);
			for (auto& expr : exprs)
			{
				call->addArgument(std::move(expr));
			}
		}

		this->currentBlock->addStatement(std::unique_ptr<IR::Statement>(call));
	}

	void handleSymbolScope(IR::Scope& scope, const SymbolScope& symbolScope)
	{
		IR::Module* module = this->context->module;
		assert(module);

		// 
		for (DeclarationSymbolSource* symbolSource : symbolScope.getDeclarations())
		{
			unique<IR::Value> val;

			Symbol* symbol = symbolSource->symbol;
			assert(symbol);
			TypeRef& type = symbol->getType();
			if (type->isTypeVariable())
			{
				// ignore type variables
				continue;
			}
			else if (type->isFunction())
			{
				val = std::make_unique<IR::Function>(type, symbol->name, symbolSource->storageQualifier == StorageQualifier::Extern);
			}
			else
			{
				auto val = std::make_unique<IR::Variable>(type);
			}

			auto* ref = scope.addReferenceable(symbol->name, symbolSource, std::move(val));
			this->context->module->cacheReferenceable(ref);			
		}
	}

	void generateConcreteStatementBody(IR::Scope* scope, AST::StatementBody* node)
	{
		SymbolScope* symbolScope = this->astContext->getScope(node);
		assert(symbolScope);
		handleSymbolScope(*scope, *symbolScope);

		auto* prevBlock = this->currentBlock;
		this->currentBlock = scope->addBlock();
		AST::visitChildren(node, this);
		this->currentBlock = prevBlock;
	}

	vector<std::unique_ptr<IR::Expression>> generateConcreteExpression(AST::Expression* expression)
	{
		ExpressionConcretizer c(this->context, this->astContext, this->function, this->currentBlock); 
		expression->accept(&c);
		return std::move(c.expressionStack);
	}

	void handleSignature(IR::Function& func, const AST::FunctionLiteral& funcLiteral)
	{
		AST::FunctionSignature* signature = funcLiteral.signature;
		for (AST::FunctionInParam* inParam : signature->inParams)
		{
			auto* symbolSource = this->astContext->getSymbolSource(inParam);
			assert(symbolSource);
			Symbol* symbol = symbolSource->getSymbol();
			assert(symbol);

			auto param = func.signature.addInParam(symbol->type, symbol->name, symbolSource);
			this->context->module->cacheReferenceable(param);
		}

		for (AST::FunctionOutParam* outParam : signature->outParams)
		{
			auto* symbolSource = this->astContext->getSymbolSource(outParam);
			assert(symbolSource);
			Symbol* symbol = symbolSource->getSymbol();
			assert(symbol);

			auto param = func.signature.addInParam(symbol->type, symbol->name, symbolSource);
			this->context->module->cacheReferenceable(param);
		}			
	}

	IR::Function* generateConcreteFunction(IR::Function& func, AST::FunctionLiteral* funcLiteral)
	{
		FunctionConcretizer c(this->context, this->astContext, &func); 

		// Handle signature
		handleSignature(func, *funcLiteral);
		
		// Handle body
		c.generateConcreteStatementBody(&func.scope, funcLiteral->body);
		return &func;
	}

	FunctionConcretizer(ConcretizerContext* context, Context* astContext, IR::Function* function)
		: context(context)
		, function(function)
		, astContext(astContext)
	{
	}

	ConcretizerContext* context = nullptr;
	IR::Function* function = nullptr;
	IR::Block* currentBlock = nullptr;
	Context* astContext = nullptr;
};

TypeRef createMainType()
{
	Type funcType = createFunctionType();
	FunctionClass& func = funcType.getFunction();

	auto ccharptrclass = std::make_unique<PrimitiveClass>(PrimitiveClass::Char, 8, PrimitiveClass::Signed);
	TypeRef cinttype = TypeRef(Type(std::make_unique<PrimitiveClass>(PrimitiveClass::Int, 32, PrimitiveClass::Signed)));
	TypeRef ccharptrtype = TypeRef(createPointerType(TypeRef(
			Type(std::make_unique<PrimitiveClass>(PrimitiveClass::Char, 8, PrimitiveClass::Signed)))));

	func.appendInParam(TypeRef(cinttype), "argc");
	func.appendInParam(TypeRef(ccharptrtype), "argv");
	func.appendOutParam(TypeRef(cinttype), "err");

	return TypeRef(std::move(funcType));
}

IR::Module concretizeASTModule(Backend::Context* backend, Context* astContext, AST::Module* astModule)
{
	assert(astModule);
	IR::Module module;
	ConcretizerContext context { backend, &module };

	auto mainType = createMainType();
	module.mainFunction = std::make_unique<IR::Function>(mainType, "main", false);

	FunctionConcretizer c(&context, astContext, &*module.mainFunction);
	c.generateConcreteStatementBody(&module.mainFunction->scope, astModule->body);
 
	// Add return instruction
	auto* val = backend->createIntegerConstant(0, 32, true);
	module.mainFunction->scope.blocks.back()->addStatement(
		std::make_unique<IR::Return>(createExpression<IR::Literal>(mainType->getFunction().outParams.back().type, val)));

	return module;
}








 