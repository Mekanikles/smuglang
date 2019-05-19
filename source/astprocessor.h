#pragma once
#include "core.h"
#include "input.h"
#include "output.h"
#include "token.h"
#include "ast.h"
#include "ir.h"
#include "evaluation/evaluation.h"
#include "evaluation.h" // TODO: Remove
#include "functions.h"
#include "declarationprocessor.h"
#include "dependencyresolver.h"

void processAST(EvaluationContext& econtext, ASTContext* context, AST::Node* root);

// Processing is done through dependency chain, rather than AST order
struct ASTProcessor : AST::Visitor
{
	void visit(AST::BinaryOp* node) override
	{
		if (this->context->processCheck(node))
			return;

		// Visit subtree of op
		AST::Visitor::visit(node);

		// TODO: Bubble up types through ops for now
		TypeRef& t1 = node->left->getType(this->context);
		TypeRef& t2 = node->right->getType(this->context);

		const auto result = unifyTypes(t1, t2);
		if (result == CannotUnify)
			assert("Cannot unify types" && false);
	}

	void visit(AST::Call* node) override
	{
		if (this->context->processCheck(node))
			return;

		// Make sure function expression is processed
		node->expr->accept(this);

		for (auto arg : node->args)
		{
			arg->accept(this);
		}

		Type& functionType = node->expr->getType(this->context);
		assert(functionType.isFunction());
		FunctionClass& function = functionType.getFunction();

		ArgumentBinding* argBinding = createFunctionArgumentBinding(*node, function);
		node->argBinding = argBinding;

		unifyFunctionCall(this->context, node, function, argBinding);

		/*
		auto& inTypes = function.inTypes;
		auto& args = node->args;

		if (function.isVariadic)
			assert(args.size() >= inTypes.size());
		else
 			assert(inTypes.size() == args.size());

		for (int i = 0, s = inTypes.size(); i < s; ++i)
		{
			Type& inType = inTypes[i];
			AST::Expression* arg = args[i];
			Type& argType = arg->getType();
			const auto result = unifyTypes(argType, inType);

			// TODO: Handle implicit casts?
			if (result == CannotUnify)
				assert("Cannot unify function argument" && false);	
		}*/	
	}

	// TODO: Remove functiondeclaration special node, use symboldeclaration instead
	void visit(AST::FunctionDeclaration* node) override
	{
		if (this->context->processCheck(node))
			return;

		auto* funcLiteral = node->funcLiteral;
		assert(funcLiteral);

		// Process literal signature separately to allow recursive 
		//	references to this declaration with the correct type
		assert(funcLiteral->signature);
		funcLiteral->signature->accept(this);
	
		// Store type for this function
		auto& signType = funcLiteral->signature->getType(this->context);
		auto& functionType = signType->getTypeVariable().type;
		Symbol* symbol = node->getSymbol(this->context);
		symbol->firstInitOrder = node->order;
		symbol->type = functionType;

		auto source = context->getSymbolSource(node);
		assert(source);

		// Store function header for evaluation
		// NOTE: We need to do this before processing of the body
		//	since we can cause recursive calls through another function.
		//	Processing this body will cause processing of the other function
		//	which will in turn require this node to have stored at least the 
		//	function header. It's complicated :(
		auto irfunction = Evaluation::createAndStoreFunctionHeader(econtext, *context, *node->funcLiteral, *source);
		assert(irfunction);

		funcLiteral->accept(this);

		// Store function body for evaluation
		Evaluation::generateFunctionBody(econtext, *context, *irfunction, *node->funcLiteral);	
	}

	void visit(AST::FunctionSignature* node) override
	{
		if (this->context->processCheck(node))
			return;

		// Visit subtree of signature
		AST::Visitor::visit(node);

		// Assign types to inparams
		for (AST::FunctionInParam* param : node->inParams)
		{
			Symbol* symbol = param->getSymbol(this->context);

			TypeRef type;
			if (param->typeExpr)
			{
				unique<IR::Literal> value = Evaluation::createLiteralFromASTExpression(this->econtext, *this->context, *param->typeExpr);
				const TypeRef& exprType = value->type;

				assert(exprType->isTypeVariable());
				type = exprType->getTypeVariable().type;
			}

			const bool isVariadic = param->isVariadic;
			if (isVariadic)
				symbol->getType() = createTupleType(std::move(type));
			else
				symbol->getType() = type;

			// Infer type from init expression
			if (param->initExpr && !isVariadic)
			{
				symbol->firstInitOrder = node->order;
				if (param->initExpr)
				{
					TypeRef& exprType = param->initExpr->getType(this->context);
					const auto result = unifyTypes(symbol->getType(), exprType);

					// TODO: Handle implicit casts?
					if (result == CannotUnify)
						assert("Cannot unify types" && false);

					// TODO: How to apply unification to expression?
				}
			}
		}

		// Assign types to outparams
		for (AST::FunctionOutParam* param : node->outParams)
		{
			Symbol* symbol = param->getSymbol(this->context);

			TypeRef type;
			if (param->typeExpr)
			{
				Value nodeVal;
				if (!evaluateExpression(this->context, param->typeExpr, &nodeVal))
				{
					assert("Cannot evaluate type expression for out-parameter" && false);
				}
				
				type = nodeVal.type->getTypeVariable().type;
			}

			symbol->type = type;
		}

		this->context->addTypeLiteral(node, node->createLiteralType(this->context));
	}

	void visit(AST::StringLiteral* node) override
	{
		if (this->context->processCheck(node))
			return;

		this->context->addTypeLiteral(node, node->createLiteralType());
	}

	void visit(AST::IntegerLiteral* node) override
	{
		if (this->context->processCheck(node))
			return;

		this->context->addTypeLiteral(node, node->createLiteralType());
	}

	void visit(AST::FloatLiteral* node) override
	{
		if (this->context->processCheck(node))
			return;

		this->context->addTypeLiteral(node, node->createLiteralType());
	}

	void visit(AST::TypeLiteral* node) override
	{
		if (this->context->processCheck(node))
			return;

		this->context->addTypeLiteral(node, node->createLiteralType());
	}

	void visit(AST::Tuple* node) override
	{
		if (this->context->processCheck(node))
			return;

		vector<TypeRef> types;
		for (auto* e : node->exprs)
		{
			e->accept(this);

			types.push_back(TypeRef(e->getType(context)));
		}

		auto type = TypeRef(createTupleType(std::move(types)));
		type.stripTrivialTuples();

		this->context->addTypeLiteral(node, std::move(type));
	}

	void visit(AST::UnaryPostfixOp* node) override
	{
		if (this->context->processCheck(node))
			return;
		
		assert(node->expr);
		node->expr->accept(this);

		Value nodeVal;
		if (!evaluateExpression(this->context, node->expr, &nodeVal))
		{
			assert("Cannot evaluate type expression for post fix operator" && false);
		}

		// TODO: Bubble up types through ops for now
		const TypeRef& t = nodeVal.type;
		TypeRef type;
		if (node->opType == TokenType::Asterisk)
		{
			type = createPointerTypeVariable(TypeRef(t.getType().getTypeVariable().type));
		}
		else
		{
			type = t;
		}

		this->context->addTypeLiteral(node, std::move(type));
	}

	void visit(AST::FunctionLiteral* node) override
	{
		if (this->context->processCheck(node))
			return;

		assert(node->signature);
		node->signature->accept(this);

		if (node->signature->outParams.empty())
		{
			m_expectedReturnType = std::optional<TypeRef>();
		}
		else
		{
			assert(node->signature->outParams.size() == 1);
			auto* pnode = node->signature->outParams[0];
			Symbol* symbol = pnode->getSymbol(this->context);
			m_expectedReturnType = symbol->type;
		}
		
		assert(node->body);		
		node->body->accept(this);
	}

	void visit(AST::ReturnStatement* node) override
	{
		if (this->context->processCheck(node))
			return;

		// TODO: This is super similiar to an assignment, maybe convert
		if (node->expr)
		{
			assert(m_expectedReturnType && "Return expression in void function");

			node->expr->accept(this);
			TypeRef& exprType = node->expr->getType(this->context);
			const auto result = unifyTypes(*m_expectedReturnType, exprType);

			// TODO: Handle implicit casts?
			if (result == CannotUnify)
				assert("Cannot unify types for return statement" && false);
		}
		else
		{
			// TODO: Is this true?
			assert(!m_expectedReturnType && "Missing return expression in non-void function");
		}
	}

	void visit(AST::TemplateDeclaration* node) override
	{
		// Do not traverse subtree
	}	

	void visit(AST::SymbolDeclaration* node) override
	{
		if (this->context->processCheck(node))
			return;

		Symbol* symbol = node->getSymbol(this->context);

		// Check explicit type

		if (node->typeExpr)
		{
			node->typeExpr->accept(this);

			unique<IR::Literal> value = Evaluation::createLiteralFromASTExpression(this->econtext, *this->context, *node->typeExpr);
			const TypeRef& exprType = value->type;

			assert(exprType->isTypeVariable());
			symbol->type = exprType->getTypeVariable().type;
		}

		// Infer type from init expression
		if (node->initExpr)
		{
			symbol->firstInitOrder = node->order;
			if (node->initExpr)
			{
				node->initExpr->accept(this);
				TypeRef& nodeType = symbol->getType();
				TypeRef& initExprType = node->initExpr->getType(this->context);
				const auto result = unifyTypes(nodeType, initExprType);

				// TODO: Handle implicit casts?
				if (result == CannotUnify)
					assert("Cannot unify types for declaration" && false);

				// TODO: How to apply unification to expression?
			}
		}
		else if (node->isExternal())
		{
			symbol->firstInitOrder = node->order;

			if (symbol->type->isFunction())
			{
				// Allow some non-concrete functions to be converted to variadics
				if (symbol->type->isFunction())
					symbol->type->getFunction().convertToVariadicIfPossible();
			}

			assert(symbol->type->isConcrete() && "External types need to be explicitly concrete");
		}

		// Evaluate defines and store as constant in IR env
		// 	This allows dependants to look them up
		if (node->isDefine())
		{
			// TODO: Hm, we probably want to store a value for types as well, a hash/id maybe?
			if (!symbol->getType()->isTypeVariable())
			{
				auto source = context->getSymbolSource(node);
				assert(source);
				assert(node->initExpr);
				Evaluation::storeConstantFromExpression(econtext, *context, *node->initExpr, *source);	
			}
		}
		else if (node->isExternal())
		{
			assert(symbol->getType()->isFunction());
			auto source = context->getSymbolSource(node);
			assert(source);
			Evaluation::storeExternal(econtext, *context, symbol->getType(), *source);
		}
	}

	void visit(AST::Assignment* node) override
	{
		if (this->context->processCheck(node))
			return;

		assert(node->symExpr);
		node->symExpr->accept(this);
		Symbol* symbol = node->symExpr->getSymbol(this->context);

		if (symbol->firstInitOrder < node->order)
			symbol->firstInitOrder = node->order;

		// Infer type from assignment
		assert(node->expr);
		node->expr->accept(this);

		TypeRef& exprType = node->expr->getType(this->context);
		const auto result = unifyTypes(symbol->type, exprType);

		// TODO: Handle implicit casts?
		if (result == CannotUnify)
			assert("Cannot unify types" && false);	
	}

	void visit(AST::SymbolExpression* node) override
	{
		if (this->context->processCheck(node))
			return;

		auto* dependency = this->context->getSymbolDependency(node);
		assert(dependency);

		// Process dependency until we are dependent on a single symbol
		//	since we are possibly dependent on a placeholder source
		auto depSource = dependency->getSymbolSource();
		while(!depSource->isSingleSymbolSource())
		{
			depSource->getNode()->accept(this);
			auto newDepSource = dependency->getSymbolSource();
			assert(depSource != newDepSource && "Could not resolve catch-all dependency! (probably a circular dependency on eval)");
			depSource = newDepSource;
		}

		// Make sure that symbol source is processed
		auto* symbolSource = dependency->getSymbolSource();
		dependency->getSymbolSource()->getNode()->accept(this);

		if (symbolSource->isTemplate())
		{
			// Process any template arguments we might have
			for (auto tArg : node->templateArgs)
			{
				tArg->accept(this);
			}

			// TODO: Literals in this list is moved into template instance constants atm
			//	make sure we can keep them around for comparisons to already existing template instances
			// Resolve constants for all template arguments
			vector<unique<IR::Literal>> tArgLiterals;
			for (auto tArg : node->templateArgs)
			{
				tArgLiterals.emplace_back(Evaluation::createLiteralFromASTExpression(this->econtext, *this->context, *tArg));
			}

			TemplateSymbolSource* templateSource = symbolSource->asTemplate();
			SymbolSource* generatedSource = nullptr;

			auto* declNode = (AST::TemplateDeclaration*)templateSource->node;

			// TODO: Match existing instances against args
			// Create new template instance
			{
				auto* currentScope = this->context->getScope(node);
				assert(currentScope);

				AST::TemplateDeclaration::Instance& instance = declNode->addInstance();		
				ASTProcessor instanceAp(this->econtext, &instance.astContext);

				// Run declaration/dependency steps on signature, 
				processDeclarations(&instance.astContext, declNode->signature, currentScope);
				resolveDependencies(&instance.astContext, declNode->signature);

				auto* argBinding = createTemplateArgumentBinding(*node, *declNode);
				assert(argBinding);

				// Match template arguments and store values
				//	NOTE: do this before further processing of signature to be able
				//		to use dependent types
				{
					auto& tParams = declNode->signature->inParams;

					for (ArgumentBinding::Param& tArgBinding : argBinding->params)
					{
						assert(tArgBinding.paramIndex < tParams.size());
						assert(tArgBinding.argIndex < tArgLiterals.size());
						AST::FunctionInParam* param = tParams[tArgBinding.paramIndex];
						SymbolSource* paramSource = instance.astContext.getSymbolSource(param);
						assert(paramSource);

						shared<IR::Literal> literal = std::move(tArgLiterals[tArgBinding.argIndex]);
						Evaluation::storeConstantFromLiteral(this->econtext, instance.astContext, literal, *paramSource);

						string name = string("tConst_") + declNode->declaration->getSymbolName() + string("_") + param->name;
						instance.literals.emplace_back(AST::TemplateDeclaration::Instance::LiteralAndSource { literal, paramSource, name});
					}

					//unifyArguments(this->context, node, argBinding);
				}

				// Process signature
				declNode->signature->accept(&instanceAp);

				// TODO: Template arguments should be unified one by one as they are processed.
				//	Now, we assume that values can be used before type checking, which can have weird consequences
				// Now we are able to unify template arguments
				bool success = unifyTemplateArguments(this->context, &instance.astContext, node, declNode->signature, argBinding);
				assert(success);

				// Signature scope should be a parent scope for the declaration
				auto* signatureScope = instance.astContext.getScope(declNode->signature);
				assert(signatureScope);		

				// Process declaration
				processDeclarations(&instance.astContext, declNode->declaration, signatureScope);
				resolveDependencies(&instance.astContext, declNode->declaration);
				declNode->declaration->accept(&instanceAp);

				generatedSource = instance.astContext.getSymbolSource(declNode->declaration);
				assert(generatedSource);
			}

			// Re-assign depedency to generated template
			assert(generatedSource);
			generatedSource->hookDependency(dependency);
		}
		else
		{
			if (node->templateArgs.size() > 0)
				assert("Template arguments were specified for non-template symbol");
		}	

		// TODO: It does not work to use firstInitOrder with evals
		// 	since they inject code, currently getting wrongly ordered
		//	Consider doing this at a later pass, in some execution-order
		//	based traversal.
		Symbol* symbol = node->getSymbol(this->context);
		if (!node->isPartOfAssignment && symbol->firstInitOrder > node->order)
		{	
			// TODO: add line/column
			printLine(string("Warning: Symbol '") + node->symbol + "' is used before initialization" + 
					"(InitOrder: " + std::to_string(symbol->firstInitOrder) + ", RefOrder: " + std::to_string(node->order) + ")");
		}
	}

	void visit(AST::EvalStatement* node) override
	{
		if (this->context->processCheck(node))
			return;

		//printLine(string("Processing eval: ") + std::to_string(node->order));

		// Make sure dependency is processed
		assert(node->expr);
		node->expr->accept(this);

		assert(!node->isGenerated);

		node->isGenerated = true;

		// TODO: This happens too early somehow, at this point we might have a multitype
		//	string for example. We need to concretizie the type here, or at least handle
		//	all possible types of strings. Either way we need to know the type first before 
		//	we can evaluate the data (different data depending on zstrings/arrays etc)
		Value nodeVal;
		if (!evaluateExpression(this->context, node->expr, &nodeVal))
		{
			assert("Cannot evaluate expression for eval statement" && false);
		}

		if (!isStringType(nodeVal.type))
		{
			assert("Expression is not of string type" && false);
		}

		// TODO: handle other strings
		assert(nodeVal.type.getType().isPointer());
		auto& type = nodeVal.type.getType().getPointer().type;
		assert(type->isPrimitive());
		assert(type->getPrimitive().isChar());
		assert(type->getSize() == 8);

		const char** text = (const char**)nodeVal.data.data();
		const uint length = strnlen(*text, 4096);
		
		// TODO: Compare with previous nodeVal
		string str = Evaluation::evaluateExpressionAsString(econtext, *context, *node->expr);
		assert(!str.compare(*text));

		printLine("Parsing eval string: "); 
		printLine(*text, 1);
 
		BufferSourceInput bufferInput(*text, length);
		Parser parser(&bufferInput);

		
		AST::Statement* statement;
		while (parser.parseStatement(&statement))
		{
			node->statements.push_back(statement);
		}

		auto* currentScope = this->context->getScope(node);
		assert(currentScope);
		
		if (parser.getParserErrors().size() != 0 && parser.getScannerErrors().size() != 0)
		{
			printLine("Eval statement contained errors:");
			printScannerErrors(parser);	
			printParserErrors(parser);
			// TODO: Handle eval errors
			assert(false);
		}

		// TODO: Introduce a single node for statement lists
		// First, add all new declarations to the current scope
		for (AST::Statement* s : node->statements)
		{
			processDeclarations(this->context, s, currentScope);
		}

		//printLine("Eval statement had these dependencies:", 1);
		//for (auto d : node->catchAllSource->dependencies)
		//	printLine(d->symbolName, 2);

		/*auto* catchAllSource = this->context->getCatchAllSymbolSource(node);
		assert(catchAllSource);

		// Now we can remove the catch-all source that we added earlier
		//	and hook up all the previously caught dependencies
		const bool wasRemoved = currentScope->removeSymbolSource(catchAllSource);
		assert(wasRemoved);

		for (auto d : catchAllSource->dependencies)
		{
			resolveDependency(d, currentScope);
		}*/

		//printLine(string("Resolving dependencies for ") + std::to_string(node->statements.size()) +
		//	" evaled statements..." , 1);

		// Resolve dependencies in newly created asts
		for (AST::Statement* s : node->statements)
		{
			resolveDependencies(this->context, s);
		}

		//printLine(string("Processing ") + std::to_string(node->statements.size()) +
		//	" evaled statements..." , 1);

		// Continue traversal
		for (AST::Statement* s : node->statements)
		{
			s->accept(this);
		}

		//printLine(string("Eval: ") + std::to_string(node->order) + " finished processing");
	}

	ASTProcessor(EvaluationContext& econtext, ASTContext* context)
		: econtext(econtext)
		, context(context)
	{
	}

	std::optional<TypeRef> m_expectedReturnType;
	EvaluationContext& econtext;
	ASTContext* context;
};

void processAST(EvaluationContext& econtext, ASTContext* context, AST::Node* root)
{
	processDeclarations(context, root);
	resolveDependencies(context, root);

	LOG("Processing ast...");
	{
		ASTProcessor ap(econtext, context);
		root->accept(&ap);
	}

	bool unresolved = false;
	for (SymbolDependency* dep : s_unresolvedDependencies)
	{
		printLine(string("Could not resolve symbol ") + dep->symbolName);
		unresolved = true;
	}
	if (unresolved)
		assert(false && "Had unresolved symbols");
}

void processAST(EvaluationContext& econtext, ASTContext* context, AST::ASTObject* ast)
{
	processAST(econtext, context, ast->module);
}
