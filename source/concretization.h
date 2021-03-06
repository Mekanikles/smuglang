#pragma once
#include "core.h"
#include "ir/ir.h"
#include "token.h"
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

template<typename T>
void writeIntValueToData(vector<u8>& data, i64 value)
{
	const T convertedValue = (T)value;
	data.resize(sizeof(T));
	memcpy(data.data(), &convertedValue, sizeof(T));
}

unique<IR::Literal> createIntegerLiteral(const TypeRef& type, i64 value)
{
	auto& primitive = type->getPrimitive();
	assert(primitive.isConcrete() && primitive.isInteger());

	vector<u8> data;
	if (primitive.Signed)
	{
		switch (primitive.size)
		{
			case 8: writeIntValueToData<i8>(data, value); break;
			case 16: writeIntValueToData<i16>(data, value); break;
			case 32: writeIntValueToData<i32>(data, value); break;
			case 64: writeIntValueToData<i64>(data, value); break;
			default:
				assert("int size not supported in literals");
		}
	}
	else
	{
		switch (primitive.size)
		{
			case 8: writeIntValueToData<u8>(data, value); break;
			case 16: writeIntValueToData<u16>(data, value); break;
			case 32: writeIntValueToData<u32>(data, value); break;
			case 64: writeIntValueToData<u64>(data, value); break;
			default:
				assert("int size not supported in literals");
		}
	}

	return createExpression<IR::Literal>(type, std::move(data));
}

unique<IR::Literal> createFloatLiteral(const TypeRef& type, double d)
{
	vector<u8> data;
	data.resize(sizeof(d));
	memcpy(data.data(), &d, sizeof(d));

	return createExpression<IR::Literal>(type, std::move(data));
}

unique<IR::Literal> createTypeLiteral(const TypeRef& type)
{
	vector<u8> data;
	data.resize(sizeof(TypeId));
	const TypeId typeId = type->typeId();
	memcpy(data.data(), &typeId, sizeof(TypeId));

	return createExpression<IR::Literal>(type, std::move(data));
}

ConcretizerContext* context = nullptr;
IR::Block* currentBlock = nullptr;
ASTContext* astContext = nullptr;

IR::Function* generateConcreteFunction(ConcretizerContext* context, ASTContext* astContext, IR::Function& func, AST::FunctionLiteral* funcLiteral);

struct ExpressionConcretizer : AST::Visitor
{
	virtual void visit(AST::Call* node) override
	{
		IR::Call* call = new IR::Call(node->getType(this->astContext));

		node->expr->accept(this);
		// TODO: How to handle multiple value expressions?
		assert(expressionStack.size() == 1);
		call->setCallable(std::move(expressionStack.back()));
		expressionStack.pop_back();

		for (auto* arg : node->args)
		{
			arg->accept(this);
			// TODO: How to handle multiple value expressions?
			assert(expressionStack.size() == 1);
			call->addArgument(std::move(expressionStack.back()));
			expressionStack.pop_back();
		}

		expressionStack.push_back(std::unique_ptr<IR::Call>(call));
	}

	virtual void visit(AST::SymbolExpression* node) override
	{
		SymbolDependency* symDep = this->astContext->getSymbolDependency(node);
		auto* source = symDep->source;
		if (source->getSymbol()->getType()->isTypeVariable())
		{
			// TODO: Atm typeliterals are not stored as refs since the type of a type
			//	carry all info we need. Should probably fix.
			expressionStack.push_back(createExpression<IR::Literal>(source->getSymbol()->getType()));
		}
		else
		{
			IR::Referenceable* ref = this->context->module->getReferenceable(source);
			assert(ref);

			expressionStack.push_back(createExpression<IR::Reference>(ref));
		}
	}

	virtual void visit(AST::ArrayAccess* node) override
	{
		node->expr->accept(this);
		// TODO: How to handle multiple value expressions?
		assert(expressionStack.size() == 1);
		auto expr = std::move(expressionStack.back());
		expressionStack.pop_back();

		node->indexExpr->accept(this);
		// TODO: How to handle multiple value expressions?
		assert(expressionStack.size() == 1);
		auto indexExpr = std::move(expressionStack.back());
		expressionStack.pop_back();

		auto& type = node->getType(this->astContext);
		expressionStack.push_back(createExpression<IR::ArrayAccess>(type, std::move(expr), std::move(indexExpr)));
	}

	virtual void visit(AST::MemberAccess* node) override
	{
		node->expr->accept(this);
		// TODO: How to handle multiple value expressions?
		assert(expressionStack.size() == 1);
		auto expr = std::move(expressionStack.back());
		expressionStack.pop_back();

		expressionStack.push_back(createExpression<IR::MemberAccess>(std::move(expr), node->getMemberName()));
	}

	string generateUniqueFunctionName(AST::FunctionLiteral* node)
	{
		return string("lambda<") + std::to_string(node->order) + ">";
	}

	virtual void visit(AST::FunctionLiteral* node) override
	{
		auto& type = node->getType(this->astContext);

		auto func = std::make_unique<IR::Function>(type, generateUniqueFunctionName(node));
		auto funcPtr = context->module->addFunction(std::move(func));
		generateConcreteFunction(this->context, this->astContext, *funcPtr, node);
		
		expressionStack.push_back(funcPtr->createLiteral());
	}

	virtual void visit(AST::FunctionSignature* node) override
	{
		auto& type = node->getType(this->astContext);

		assert(type->isTypeVariable());

		expressionStack.push_back(createTypeLiteral(type));
	}

	virtual void visit(AST::TypeLiteral* node) override
	{
		auto& type = node->getType(this->astContext);

		assert(type->isTypeVariable());

		expressionStack.push_back(createTypeLiteral(type));
	}

	virtual void visit(AST::IntegerLiteral* node) override
	{
		TypeRef& type = node->getType(this->astContext);

		// Defaulting rules
		{
			if (type->isMultiType())
			{
				// Try unifying with int as default to c compatibility
				auto intType = TypeRef(createPrimitiveType(PrimitiveClass::Int));
				unifyTypes(type, intType);
			}

			assert(type.getType().ensureConcrete());
		}

		assert(type.getType().isPrimitive());
		const PrimitiveClass& primitive = type.getType().getPrimitive();
		assert(primitive.primitiveType == PrimitiveClass::Int || primitive.primitiveType == PrimitiveClass::Float);

		// TODO: Replace IntegerLiteral with something that can handle floats as well
		if (primitive.primitiveType == PrimitiveClass::Float)
		{
			std::istringstream os(node->value);
			double d;
			os >> d;

			expressionStack.push_back(createFloatLiteral(type, d));
		}
		else
		{			
			i64 l;
			if (node->ltype == AST::IntegerLiteral::Hexadecimal)
			{
				string s = &node->value[2];
				l = std::stoll(s, nullptr, 16);
			}
			else if (node->ltype == AST::IntegerLiteral::Binary)
			{
				string s = &node->value[2];
				l = std::stoll(s, nullptr, 2);
			}	
			else
			{
				l = std::stoll(node->value, nullptr, 10);
			}

			expressionStack.push_back(createIntegerLiteral(type, l));
		}
	}

	void visit(AST::FloatLiteral* node) override
	{
		TypeRef& type = node->getType(this->astContext);

		// Defaulting rules
		{
			assert(type.getType().ensureConcrete());
		}

		assert(type.getType().isPrimitive());
		const PrimitiveClass& primitive = type.getType().getPrimitive();
		assert(primitive.primitiveType == PrimitiveClass::Float);

		//auto* value = this->context->backend->createFloatConstantFromText(node->value, primitive.size);

		std::istringstream os(node->value);
    	double d;
    	os >> d;

		expressionStack.push_back(createFloatLiteral(type, d));
	}	

	virtual void visit(AST::StringLiteral* node) override
	{
		TypeRef& type = node->getType(this->astContext);

		// Defaulting rules
		{
			if (type->isMultiType())
			{
				// Try unifying with char* as default to c compatibility
				auto ptrType = TypeRef(createPointerType(createPrimitiveType(PrimitiveClass::Char)));
				unifyTypes(type, ptrType);
			}

			assert(type.getType().ensureConcrete());
		}
		
		assert(isStringType(type));

		// Might as well create the value here directly
		const string str = processQuotedInputString(node->value);
		//const bool isCString = type->isPointer();

		//auto* value = this->context->backend->createStringConstantFromText(str);

		vector<u8> data;
		const char* cstr = str.c_str();
		uint length = str.length() + 1;
		data.resize(length);
		memcpy(data.data(), cstr, length);

		expressionStack.push_back(createExpression<IR::Literal>(type, std::move(data)));
	}

	virtual void visit(AST::UnaryPostfixOp* node) override
	{
		if (node->opType == TokenType::Asterisk)
		{
			// No need to visit sub-expression, we already know the type
			auto& type = node->getType(this->astContext);
			assert(type->isTypeVariable());
			expressionStack.push_back(createTypeLiteral(type));
		}
		else
		{
			assert("Unknown postfix operator");
		}
	}

	virtual void visit(AST::UnaryOp* node) override
	{
		node->expr->accept(this);
		// TODO: How to handle multiple value expressions?
		assert(expressionStack.size() == 1);
		auto expr = std::move(expressionStack.back());
		expressionStack.pop_back();

		IR::UnaryOp::OpType opType;
		switch (node->opType)
		{
			case TokenType::Minus: opType = IR::UnaryOp::Neg; break;
			default: assert(false && "Invalid op type");
		}

		auto& type = node->getType(this->astContext);		

		expressionStack.push_back(createExpression<IR::UnaryOp>(
				type,
				opType,
				std::move(expr)
			));
	}

	virtual void visit(AST::BinaryOp* node) override
	{
		node->left->accept(this);
		// TODO: How to handle multiple value expressions?
		assert(expressionStack.size() == 1);
		auto leftEpxr = std::move(expressionStack.back());
		expressionStack.pop_back();

		node->right->accept(this);
		// TODO: How to handle multiple value expressions?
		assert(expressionStack.size() == 1);
		auto rightEpxr = std::move(expressionStack.back());
		expressionStack.pop_back();

		IR::BinaryOp::OpType opType;
		switch (node->opType)
		{
			case TokenType::Plus: opType = IR::BinaryOp::Add; break;
			case TokenType::Minus: opType = IR::BinaryOp::Sub; break;
			case TokenType::Asterisk: opType = IR::BinaryOp::Mul; break;
			case TokenType::Slash: opType = IR::BinaryOp::Div; break;
			case TokenType::Modulo: opType = IR::BinaryOp::Mod; break;
			case TokenType::EqualsOp: opType = IR::BinaryOp::Eq; break;
			case TokenType::LessThanOp: opType = IR::BinaryOp::LT; break;
			case TokenType::GreaterThanOp: opType = IR::BinaryOp::GT; break;
			case TokenType::LessThanOrEqualsOp: opType = IR::BinaryOp::LTE; break;
			case TokenType::GreaterThanOrEqualsOp: opType = IR::BinaryOp::GTE; break;
			default: assert(false && "Invalid op type");
		}

		auto& type = node->getType(this->astContext);

		expressionStack.push_back(createExpression<IR::BinaryOp>(
				type,
				opType,
				std::move(leftEpxr),
				std::move(rightEpxr)
			));
	}

	unique<IR::Expression> concretizeExpression(AST::Expression& expr)
	{
		expr.accept(this);

		// TODO: Handle multiple return values
		assert(expressionStack.size() == 1);
		return std::move(expressionStack.back());
	}

	ExpressionConcretizer(ConcretizerContext* context, ASTContext* astContext)
		: context(context)
		, astContext(astContext)
	{
	}

	ConcretizerContext* context;
	ASTContext* astContext;
	vector<std::unique_ptr<IR::Expression>> expressionStack;
};

void createAndAddGlobal(ConcretizerContext& context, const TypeRef& type, SymbolSource& source)
{
	Symbol* symbol = source.getSymbol();
	auto variable = std::make_unique<IR::Variable>(type, symbol->name, &source);
	context.module->addGlobal(std::move(variable));
}

void createAndAddExternal(ConcretizerContext& context, const TypeRef& type, SymbolSource& source)
{
	Symbol* symbol = source.getSymbol();
	std::shared_ptr<IR::StaticLinkable> linkable;
	IR::External* existingExternal = context.module->getExternalByName(symbol->name);
	if (existingExternal)
	{
		auto& exType = existingExternal->getType();
		assert(exType == type && "Cannot declare c-externals with same name but different types");

		linkable = existingExternal->linkable;
	}
	else
	{
		linkable = std::make_shared<IR::StaticLinkable>(type, symbol->name);
	}

	auto external = std::make_unique<IR::External>(linkable, &source);
	context.module->addExternal(std::move(external));
}

// TODO: StatementConcretizer?
struct FunctionConcretizer : AST::Visitor
{
	virtual void visit(AST::FunctionDeclaration* node) override
	{
		auto* ss = astContext->getSymbolSource(node);
		assert(ss);
		auto* ref = this->context->module->getReferenceable(ss);
		assert(ref);

		auto& constant = ref->asConstant();
		IR::Function* func = context->module->getFunction(constant.literal->readValue<IR::FunctionId>());
		assert(func);

		generateConcreteFunction(this->context, this->astContext, *func, node->funcLiteral);
	}

	virtual void visit(AST::SymbolDeclaration* node) override
	{
		if (node->storageQualifier == StorageQualifier::Extern)
			return;

		if (node->getType(this->astContext)->isTypeVariable())
			return;

		if (node->initExpr)
		{
			auto exprs = generateConcreteExpression(node->initExpr);

			// TODO: How to handle multiple return values?
			assert(exprs.size() == 1);

			SymbolSource* source = this->astContext->getSymbolSource(node);
			assert(source);

			IR::Referenceable* ref = this->context->module->getReferenceable(source);
			assert(ref);		

			// Create new ref expression, since we know that we want to reference the declared symbol here
			auto refExpr = createExpression<IR::Reference>(ref);
			this->currentBlock->addStatement(std::make_unique<IR::Assignment>(std::move(refExpr), std::move(exprs.back())));
		}
	}

	virtual void visit(AST::Assignment* node) override
	{
		auto assExprs = generateConcreteExpression(node->target);
		assert(assExprs.size() == 1);

		auto valExprs = generateConcreteExpression(node->expr);
		assert(valExprs.size() == 1);		

		this->currentBlock->addStatement(std::make_unique<IR::Assignment>(std::move(assExprs.back()), std::move(valExprs.back())));		
	}	

	virtual void visit(AST::IfStatement* node) override
	{
		auto condExprs = generateConcreteExpression(node->expr);
		assert(condExprs.size() == 1);

		auto cond = std::make_unique<IR::Conditional>(std::move(condExprs.back()));

		auto* prevBlock = this->currentBlock;
		this->currentBlock = cond->trueScope.addBlock();
		node->statement->accept(this);

		if (node->elseStatement)
		{
			this->currentBlock = cond->falseScope.addBlock();
			node->elseStatement->accept(this);
		}

		this->currentBlock = prevBlock;

		auto* condptr = cond.get();
		this->currentBlock->addStatement(std::move(cond));

		// Manually propagate statement index to sub scopes
		// TODO: Improve this
		condptr->trueScope.scopeLocalIndex = condptr->scopeLocalIndex;
		condptr->falseScope.scopeLocalIndex = condptr->scopeLocalIndex;
	}

	virtual void visit(AST::DeferStatement* node) override
	{
		auto irDefer = std::make_unique<IR::Defer>();
		this->currentScope->trackDeferStatement(irDefer.get());

		auto* prevScope = this->currentScope;
		this->currentScope = &irDefer->scope;

		auto* prevBlock = this->currentBlock;
		this->currentBlock = this->currentScope->addBlock();
		node->statement->accept(this);
		this->currentBlock = prevBlock;
		
		this->currentScope = prevScope;

		auto* deferptr = irDefer.get();
		this->currentBlock->addStatement(std::move(irDefer));

		// Manually propagate statement index to sub scopes
		// TODO: Improve this
		deferptr->scope.scopeLocalIndex = deferptr->scopeLocalIndex;
	}

	virtual void visit(AST::LoopStatement* node) override
	{
		auto irLoop = std::make_unique<IR::Loop>();

		auto* prevScope = this->currentScope;
		this->currentScope = &irLoop->scope;

		auto* prevBlock = this->currentBlock;
		this->currentBlock = this->currentScope->addBlock();
		node->statement->accept(this);
		this->currentBlock = prevBlock;
		
		this->currentScope = prevScope;

		auto* loopptr = irLoop.get();
		this->currentBlock->addStatement(std::move(irLoop));

		// Manually propagate statement index to sub scopes
		// TODO: Improve this
		loopptr->scope.scopeLocalIndex = loopptr->scopeLocalIndex;
	}

	virtual void visit(AST::ContinueStatement* node) override
	{
		auto cont = std::make_unique<IR::Continue>();
		this->currentBlock->addStatement(std::move(cont));			
	}

	virtual void visit(AST::BreakStatement* node) override
	{
		auto br = std::make_unique<IR::Break>();
		this->currentBlock->addStatement(std::move(br));				
	}

	virtual void visit(AST::StatementBody* node) override
	{
		auto* scope = this->currentBlock->addStatement(std::make_unique<IR::Scope>());
		
		generateScopedStatement(scope, node);
	}

	virtual void visit(AST::ReturnStatement* node) override
	{
		if (node->expr)
		{
			auto exprs = generateConcreteExpression(node->expr);

			// TODO: How to handle multiple return values?
			assert(exprs.size() == 1);

			this->currentBlock->addStatement(std::make_unique<IR::Return>(std::move(exprs.back())));
		}
		else
		{
			this->currentBlock->addStatement(std::make_unique<IR::Return>());
		}
	}

	virtual void visit(AST::Call* node) override
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

	virtual void visit(AST::TemplateDeclaration* node) override
	{
		for (auto& instance : node->instances)
		{
			FunctionConcretizer concretizer(this->context, &instance.astContext, currentScope, currentBlock);
			concretizer.generateConcreteNode(node->declaration);
		}
	}

	void handleDeclarationSymbolSource(IR::Scope& scope, DeclarationSymbolSource& source)
	{
		Symbol* symbol = source.getSymbol();
		assert(symbol);
		TypeRef& type = symbol->getType();
		if (type->isTypeVariable())
			return;

		if (source.storageQualifier == StorageQualifier::Extern)
		{
			createAndAddExternal(*this->context, type, source);
		}
		else if (source.storageQualifier == StorageQualifier::Def)
		{
			if (type->isFunction())
			{
				auto func = std::make_unique<IR::Function>(type, symbol->name);
				auto funcPtr = this->context->module->addFunction(std::move(func));

				this->context->module->addConstant(funcPtr->createConstant(&source));
			}
			else
			{
				// TODO: For now, treat defs as global variables, until we enfore conversion to literals for all defs
				createAndAddGlobal(*this->context, type, source);
			}	
		}
		else
		{	
			if (source.isStatic)
			{
				createAndAddGlobal(*this->context, type, source);
			}
			else
			{
				auto variable = std::make_unique<IR::Variable>(type, symbol->name, &source);
				auto* ref = scope.addVariable(std::move(variable));
				this->context->module->cacheReferenceable(ref);
			}
		}
	}

	void handleSymbolScope(IR::Scope& scope, const SymbolScope& symbolScope)
	{
		IR::Module* module = this->context->module;
		assert(module);

		for (auto* symbolSource : symbolScope.getDeclarations())
		{
			if (symbolSource->isTemplate())
			{
				auto* templateSource = static_cast<TemplateSymbolSource*>(symbolSource);
				auto* templateNode = static_cast<AST::TemplateDeclaration*>(templateSource->node);
				assert(templateNode);

				for (auto& instance : templateNode->instances)
				{
					// Store template constants
					for (auto& literalAndSource : instance.literals)
					{
						if (literalAndSource.literal->type->isTypeVariable())
							continue;

						auto constant = std::make_unique<IR::Constant>(literalAndSource.literal, literalAndSource.name, literalAndSource.source);
						this->context->module->addConstant(std::move(constant));			
					}

					// Add instanced declaration to current scope
					auto* declSymbolSource = instance.astContext.getSymbolSource(templateNode->declaration);
					assert(declSymbolSource);
					assert(declSymbolSource->isSingleSymbolSource() && !declSymbolSource->isTemplate());
					auto* declarationSymbolSource = static_cast<DeclarationSymbolSource*>(declSymbolSource);
					handleDeclarationSymbolSource(scope, *declarationSymbolSource);
				}
			}
			else
			{
				assert(symbolSource->isSingleSymbolSource());
				auto* declarationSymbolSource = static_cast<DeclarationSymbolSource*>(symbolSource);
				handleDeclarationSymbolSource(scope, *declarationSymbolSource);
			}
		}
	}

	void generateConcreteNode(AST::Node* node)
	{
		node->accept(this);
	}

	void generateScopedStatement(IR::Scope* scope, AST::Statement* statement)
	{
		SymbolScope* symbolScope = this->astContext->getScope(statement);
		assert(symbolScope);
		handleSymbolScope(*scope, *symbolScope);

		auto* prevScope = this->currentScope;
		this->currentScope = scope;

		auto* prevBlock = this->currentBlock;
		this->currentBlock = scope->addBlock();
		AST::visitChildren(statement, this);
		this->currentBlock = prevBlock;
		
		this->currentScope = prevScope;
	}

	vector<std::unique_ptr<IR::Expression>> generateConcreteExpression(AST::Expression* expression)
	{
		ExpressionConcretizer c(this->context, this->astContext); 
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

			auto param = func.getSignature().addInParam(symbol->type, symbol->name, symbolSource);
			this->context->module->cacheReferenceable(param);
		}

		for (AST::FunctionOutParam* outParam : signature->outParams)
		{
			auto* symbolSource = this->astContext->getSymbolSource(outParam);
			assert(symbolSource);
			Symbol* symbol = symbolSource->getSymbol();
			assert(symbol);

			auto param = func.getSignature().addOutParam(symbol->type, symbol->name, symbolSource);
			this->context->module->cacheReferenceable(param);
		}			
	}

	FunctionConcretizer(ConcretizerContext* context, ASTContext* astContext, IR::Scope* currentScope = nullptr, IR::Block* currentBlock = nullptr)
		: context(context)
		, astContext(astContext)
		, currentScope(currentScope)
		, currentBlock(currentBlock)
	{
	}

	ConcretizerContext* context = nullptr;
	ASTContext* astContext = nullptr;	
	IR::Scope* currentScope = nullptr;
	IR::Block* currentBlock = nullptr;
};

IR::Function* generateConcreteFunction(ConcretizerContext* context, ASTContext* astContext, IR::Function& func, AST::FunctionLiteral* funcLiteral)
{
	FunctionConcretizer c(context, astContext); 

	// Handle signature
	c.handleSignature(func, *funcLiteral);
	
	// Handle body
	c.generateScopedStatement(&func.getScope(), funcLiteral->body);
	return &func;
}

TypeRef createLocalMainType()
{
	Type funcType = createFunctionType();
	return TypeRef(std::move(funcType));
}

IR::Module concretizeASTModule(Backend::Context* backend, ASTContext* astContext, AST::Module* astModule)
{
	assert(astModule);
	IR::Module module;
	ConcretizerContext context { backend, &module };

	// Local main
	{
		auto localMainType = createLocalMainType();
		module.localMain = std::make_unique<IR::Function>(localMainType, "__localmain");

		FunctionConcretizer c(&context, astContext);
		// TODO: Handle signature
		c.generateScopedStatement(&module.localMain->scope, astModule->body);
	}

	return module;
}








 