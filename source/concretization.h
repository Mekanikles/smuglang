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

unique<IR::Literal> createIntegerLiteral(const TypeRef& type, long long l)
{
	vector<u8> data;
	data.resize(sizeof(l));
	memcpy(data.data(), &l, sizeof(l));

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

		IR::Referenceable* ref = this->context->module->getReferenceable(source);
		assert(ref);

		expressionStack.push_back(createExpression<IR::Reference>(ref));
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

	virtual void visit(AST::IntegerLiteral* node) override
	{
		TypeRef& type = node->getType(this->astContext);

		// Defaulting rules
		{
			assert(type.getType().ensureConcrete());
		}

		assert(type.getType().isPrimitive());
		const PrimitiveClass& primitive = type.getType().getPrimitive();
		assert(primitive.primitiveType == PrimitiveClass::Int);

		// Might as well create the value here directly
		//auto* value = this->context->backend->createIntegerConstantFromText(node->value, primitive.size, primitive.signedType == PrimitiveClass::Signed);

		std::istringstream os(node->value);
    	long long l;
    	os >> l;

		expressionStack.push_back(createIntegerLiteral(type, l));
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

		vector<u8> data;
		data.resize(sizeof(d));
		memcpy(data.data(), &d, sizeof(d));
	
		expressionStack.push_back(createExpression<IR::Literal>(type, std::move(data)));
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
		auto assExprs = generateConcreteExpression(node->symExpr);
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
		this->currentBlock = &cond->trueBlock;
		node->statement->accept(this);

		if (node->elseStatement)
		{
			this->currentBlock = &cond->falseBlock;
			node->elseStatement->accept(this);
		}

		this->currentBlock = prevBlock;

		this->currentBlock->addStatement(std::move(cond));				
	}	

	virtual void visit(AST::StatementBody* node) override
	{
		auto* scope = this->currentBlock->addStatement(std::make_unique<IR::Scope>());
		
		generateConcreteStatementBody(scope, node);
	}

	virtual void visit(AST::ReturnStatement* node) override
	{
		auto exprs = generateConcreteExpression(node->expr);

		// TODO: How to handle multiple return values?
		assert(exprs.size() == 1);

		this->currentBlock->addStatement(std::make_unique<IR::Return>(std::move(exprs.back())));
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

	void handleSymbolScope(IR::Scope& scope, const SymbolScope& symbolScope)
	{
		IR::Module* module = this->context->module;
		assert(module);

		// 
		for (DeclarationSymbolSource* symbolSource : symbolScope.getDeclarations())
		{
			Symbol* symbol = symbolSource->symbol;
			assert(symbol);
			TypeRef& type = symbol->getType();
			if (type->isTypeVariable())
				continue;

			if (symbolSource->storageQualifier == StorageQualifier::Extern)
			{
				createAndAddExternal(*this->context, type, *symbolSource);
			}
			else if (symbolSource->storageQualifier == StorageQualifier::Def)
			{
				if (type->isFunction())
				{
					auto func = std::make_unique<IR::Function>(type, symbol->name);
					auto funcPtr = this->context->module->addFunction(std::move(func));

					this->context->module->addConstant(funcPtr->createConstant(symbolSource));
				}
				else
				{
					// TODO: For now, treat defs as variables, until we enfore conversion to literals for all defs
					auto variable = std::make_unique<IR::Variable>(type, symbol->name, symbolSource);
					auto* ref = scope.addVariable(std::move(variable));
					this->context->module->cacheReferenceable(ref);	
				}	
			}
			else
			{
				auto variable = std::make_unique<IR::Variable>(type, symbol->name, symbolSource);
				auto* ref = scope.addVariable(std::move(variable));
				this->context->module->cacheReferenceable(ref);	
			}	
		}
	}

	void generateConcreteStatementBody(IR::Scope* scope, AST::StatementBody* node)
	{
		SymbolScope* symbolScope = this->astContext->getScope(node);
		assert(symbolScope);
		handleSymbolScope(*scope, *symbolScope);

		auto* prevScope = this->currentScope;
		this->currentScope = scope;

		auto* prevBlock = this->currentBlock;
		this->currentBlock = scope->addBlock();
		AST::visitChildren(node, this);
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

	FunctionConcretizer(ConcretizerContext* context, ASTContext* astContext)
		: context(context)
		, astContext(astContext)
	{
	}

	ConcretizerContext* context = nullptr;
	IR::Scope* currentScope = nullptr;
	IR::Block* currentBlock = nullptr;
	ASTContext* astContext = nullptr;
};

IR::Function* generateConcreteFunction(ConcretizerContext* context, ASTContext* astContext, IR::Function& func, AST::FunctionLiteral* funcLiteral)
{
	FunctionConcretizer c(context, astContext); 

	// Handle signature
	c.handleSignature(func, *funcLiteral);
	
	// Handle body
	c.generateConcreteStatementBody(&func.getScope(), funcLiteral->body);
	return &func;
}

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

IR::Module concretizeASTModule(Backend::Context* backend, ASTContext* astContext, AST::Module* astModule)
{
	assert(astModule);
	IR::Module module;
	ConcretizerContext context { backend, &module };

	auto mainType = createMainType();
	module.main = std::make_unique<IR::Function>(mainType, "main");

	FunctionConcretizer c(&context, astContext);
	// TODO: Handle signature
	c.generateConcreteStatementBody(&module.main->scope, astModule->body);
 
	// Add return instruction
	auto& returnType = mainType->getFunction().outParams.back().type;
	module.main->scope.blocks.back()->addStatement(
		std::make_unique<IR::Return>(createIntegerLiteral(returnType, 0)));

	return module;
}








 