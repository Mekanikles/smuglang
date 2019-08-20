#pragma once
#include "core.h"
#include "input.h"
#include "output.h"
#include "token.h"
#include "ast.h"
#include "ir/ir.h"
#include "evaluation/evaluation.h"
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

		// TODO: Can we stop trivial tuples form happening?
		// Strip any tuples we might have
		t1 = t1.stripTrivialWrapperTypes();
		t2 = t2.stripTrivialWrapperTypes();

		const auto result = unifyTypes(t1, t2);
		if (!result)
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

		bool success = unifyFunctionCall(this->context, node, function, argBinding);
		assert(success && "Could not unify function argument");
	}

	// TODO: This is very similar to function/template params
	void visit(AST::StructField* node) override
	{
		if (this->context->processCheck(node))
			return;

		Symbol* symbol = node->getSymbol(this->context);

		TypeRef type;
		if (node->typeExpr)
		{
			node->typeExpr->accept(this);

			unique<IR::Literal> value = Evaluation::createLiteralFromASTExpression(this->econtext, *this->context, *node->typeExpr);
			const TypeRef& exprType = value->type;

			assert(exprType->isTypeVariable());
			// Note: Clone type here so that any inference is not done on the source
			type = exprType->getTypeVariable().type.clone();
		}

		// Infer type from init expression
		if (node->initExpr)
		{
			node->initExpr->accept(this);

			TypeRef& exprType = node->initExpr->getType(this->context);
			const auto result = unifyTypes(type, exprType);

			// TODO: Handle implicit casts?
			if (!result)
				assert("Cannot unify types" && false);

			// TODO: How to apply unification to expression?
		}

		// Assign type
		symbol->getType() = type;
	}

	// TODO: Remove structdeclaration special node, use symboldeclaration instead
	void visit(AST::StructDeclaration* node) override
	{
		if (this->context->processCheck(node))
			return;

		// Visit subtree of signature
		AST::Visitor::visit(node);

		TypeRef literalType = node->createLiteralType(this->context);
		TypeRef nodeType = node->getSymbol(this->context)->getType();

		// TODO: this feels unnecessary, but stuff might have bound to the node type at decl resolving
		unifyTypes(nodeType, literalType);

		this->context->addTypeLiteral(node, std::move(nodeType));	
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

	void visit(AST::FunctionInParam* node) override
	{
		if (this->context->processCheck(node))
			return;

		Symbol* symbol = node->getSymbol(this->context);

		TypeRef type;
		if (node->typeExpr)
		{
			node->typeExpr->accept(this);

			unique<IR::Literal> value = Evaluation::createLiteralFromASTExpression(this->econtext, *this->context, *node->typeExpr);
			const TypeRef& exprType = value->type;

			assert(exprType->isTypeVariable());
			// Note: Clone type here so that any inference is not done on the source
			type = exprType->getTypeVariable().type.clone();
		}

		const bool isVariadic = node->isVariadic;
		if (isVariadic)
			type = createTupleType(std::move(type));

		// Infer type from init expression
		if (node->initExpr)
		{
			node->initExpr->accept(this);

			assert(!isVariadic);
			symbol->firstInitOrder = node->order;
			if (node->initExpr)
			{
				TypeRef& exprType = node->initExpr->getType(this->context);
				const auto result = unifyTypes(type, exprType);

				// TODO: Handle implicit casts?
				if (!result)
					assert("Cannot unify types" && false);

				// TODO: How to apply unification to expression?
			}
		}

		// TODO: Have separate node for template params, so we can assert on expression
		const ASTContext::ExpressionAndContext& exprAndContext = this->context->getTemplateExpression(node);
		if (AST::Expression* expr = exprAndContext.expr)
		{
			// Unify each expression as they are used to allow type inference on template args
			TypeRef& exprType = expr->getType(exprAndContext.context);
			const auto result = unifyTypes(exprType, type);
			if (!result)
				assert("Cannot unify types" && false);

			// Expression value must be known at this time
			shared<IR::Literal> literal = Evaluation::createLiteralFromASTExpression(this->econtext, *exprAndContext.context, *expr);

			// Store constant
			Evaluation::storeConstantFromLiteral(this->econtext, *this->context, literal, *this->context->getSymbolSource(node));		

			// Template declaration needs to store this literal, save it
			this->context->addTemplateLiteral(node, literal);
		}

		// Assign type
		symbol->getType() = type;	
	}

	void visit(AST::FunctionOutParam* node) override
	{
		if (this->context->processCheck(node))
			return;

		Symbol* symbol = node->getSymbol(this->context);

		TypeRef type;
		if (node->typeExpr)
		{
			node->typeExpr->accept(this);

			unique<IR::Literal> value = Evaluation::createLiteralFromASTExpression(this->econtext, *this->context, *node->typeExpr);
			const TypeRef& exprType = value->type;

			assert(exprType->isTypeVariable());
			// Note: Clone type here so that any inference is not done on the source
			type = exprType->getTypeVariable().type.clone();
		}

		symbol->type = type;			
	}

	void visit(AST::FunctionSignature* node) override
	{
		if (this->context->processCheck(node))
			return;

		// Visit subtree of signature
		AST::Visitor::visit(node);

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
		type.stripTrivialWrapperTypes();

		this->context->addTypeLiteral(node, std::move(type));
	}

	void visit(AST::UnaryPostfixOp* node) override
	{
		if (this->context->processCheck(node))
			return;
		
		assert(node->expr);
		node->expr->accept(this);

		if (node->opType == TokenType::Asterisk)
		{
			// Pointer-fication of types requires the internal type to be known at compile time
			//	TODO: Does this work for generics?
			unique<IR::Literal> value = Evaluation::createLiteralFromASTExpression(this->econtext, *this->context, *node->expr);
			const TypeRef& exprType = value->type;

			assert(exprType->isTypeVariable());
			// Note: Clone type here so that any inference is not done on the source
			TypeRef type = createPointerTypeVariable(exprType->getTypeVariable().type.clone());
			this->context->addTypeLiteral(node, std::move(type));
		}
		else
		{
			assert(false && "Unknown postfix operator");
		}
	}

	void visit(AST::FunctionLiteral* node) override
	{
		if (this->context->processCheck(node))
			return;

		assert(node->signature);
		node->signature->accept(this);

		if (node->signature->outParams.empty())
		{
			m_expectedReturnTypeStack.push_back(std::optional<TypeRef>());
		}
		else
		{
			assert(node->signature->outParams.size() == 1);
			auto* pnode = node->signature->outParams[0];
			Symbol* symbol = pnode->getSymbol(this->context);
			m_expectedReturnTypeStack.push_back(symbol->type);
		}
		
		assert(node->body);		
		node->body->accept(this);

		m_expectedReturnTypeStack.pop_back();
	}

	void visit(AST::ReturnStatement* node) override
	{
		if (this->context->processCheck(node))
			return;

		// TODO: This is super similiar to an assignment, maybe convert
		if (node->expr)
		{
			assert(m_expectedReturnTypeStack.back() && "Return expression in void function");

			node->expr->accept(this);
			TypeRef& exprType = node->expr->getType(this->context);
			TypeRef& expectedType = *m_expectedReturnTypeStack.back();
			auto result = generateTypeUnification(exprType, expectedType, DisallowRightChange);

			if (!result)
				assert("Cannot unify types for return statement" && false);

			assert(!result.castNeeded());
			result.apply();
		}
		else
		{
			// TODO: Is this true?
			assert(!m_expectedReturnTypeStack.back() && "Missing return expression in non-void function");
		}
	}

	void visit(AST::TemplateDeclaration* node) override
	{
		// Do not traverse subtree
	}	

	void visit(AST::ArrayAccess* node) override
	{
		if (this->context->processCheck(node))
			return;

		node->expr->accept(this);

		if (node->indexExpr)
			node->indexExpr->accept(this);

		TypeRef& exprType = node->expr->getType(this->context);

		if (exprType->isTypeVariable())
		{
			this->context->addTypeLiteral(node, createTypeVariable());
		}
		else
		{
			auto& array = exprType->getArray();	
			// Hm, if we want type inference on array elements
			//	we should not clone the type here
			this->context->addTypeLiteral(node, array.type.clone());
		};
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
			// Note: Clone type here so that any inference is not done on the source
			symbol->type = exprType->getTypeVariable().type.clone();
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
				if (!result)
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
		else if (node->isStatic)
		{
			// TODO: Hm, we probably want to store a value for types as well, a hash/id maybe?
			if (!symbol->getType()->isTypeVariable())
			{
				auto source = context->getSymbolSource(node);
				assert(source);
				assert(node->initExpr);
				
				Evaluation::storeGlobal(econtext, symbol->getType(), *source);	
			}
		}
		else if (node->isExternal())
		{
			assert(symbol->getType()->isFunction());
			auto source = context->getSymbolSource(node);
			assert(source);
			Evaluation::storeExternal(econtext, symbol->getType(), *source);
		}
	}

	void visit(AST::Assignment* node) override
	{
		if (this->context->processCheck(node))
			return;

		assert(node->target);
		node->target->accept(this);

		assert(node->expr);
		node->expr->accept(this);

		if (node->target->isSymbolExpression())
		{
			AST::SymbolExpression* symExpr = static_cast<AST::SymbolExpression*>(node->target);

			Symbol* symbol = symExpr->getSymbol(this->context);

			if (symbol->firstInitOrder < node->order)
				symbol->firstInitOrder = node->order;

			// Infer type from assignment
			TypeRef& exprType = node->expr->getType(this->context);
			const auto result = unifyTypes(symbol->type, exprType);

			// TODO: Handle implicit casts?
			if (!result)
				assert("Cannot unify types" && false);
		}
		else if (node->target->isMemberAccess())
		{
			AST::MemberAccess* maccess = static_cast<AST::MemberAccess*>(node->target);
			assert(maccess->expr->isSymbolExpression());
			{
				AST::SymbolExpression* symExpr = static_cast<AST::SymbolExpression*>(maccess->expr);
				Symbol* symbol = symExpr->getSymbol(this->context);

				TypeRef& targetType = symbol->getType();
				if (targetType->isStruct())
				{
					StructClass& structType = targetType->getStruct();
					string& memberName = maccess->member->symbol;

					auto* field = structType.getFieldByName(memberName);
					assert(field && "No member found by specified name");

					// Infer type from assignment
					TypeRef& exprType = node->expr->getType(this->context);
					const auto result = unifyTypes(exprType, field->type, DisallowRightChange);

					// TODO: Handle implicit casts?
					if (!result)
						assert("Cannot unify types" && false);
				}
				else
				{
					assert(false);
				}
			}
		}
		else if (node->target->isArrayAccess())
		{
			AST::ArrayAccess* arrAcc = static_cast<AST::ArrayAccess*>(node->target);

			TypeRef& arrBaseType = arrAcc->expr->getType(this->context);
			assert(!arrBaseType->isTypeVariable() && "Cannot write to array access on type");
			TypeRef& arrayElementType = arrAcc->getType(this->context);

			// Infer type from assignment
			TypeRef& exprType = node->expr->getType(this->context);
			const auto result = unifyTypes(exprType, arrayElementType);

			// TODO: Handle implicit casts?
			if (!result)
				assert("Cannot unify types" && false);	
		}	
		else
		{
			assert(false && "Left of assignment needs to be supported expression");
		}
	}

	void visit(AST::SymbolExpression* node) override
	{
		if (this->context->processCheck(node))
			return;

		auto* dependency = this->context->getSymbolDependency(node);
		assert(dependency);

		// Process dependency until we are dependent on a single symbol
		//	since we are possibly dependent on a placeholder source
		auto depSource = dependency->getHookedSymbolSource();
		while(!depSource->isSingleSymbolSource())
		{
			process(depSource->getNode());
			auto newDepSource = dependency->getHookedSymbolSource();
			assert(depSource != newDepSource && "Could not resolve catch-all dependency! (probably a circular dependency on eval)");
			depSource = newDepSource;
		}

		// Make sure that symbol source is processed
		auto* symbolSource = dependency->getHookedSymbolSource();
		process(symbolSource->getNode());

		if (symbolSource->isTemplate())
		{
			// Process any template arguments we might have
			for (auto tArg : node->templateArgs)
			{
				tArg->accept(this);
			}

			TemplateSymbolSource* templateSource = symbolSource->asTemplate();

			auto* declNode = (AST::TemplateDeclaration*)templateSource->node;
			auto* argBinding = createTemplateArgumentBinding(*node, *declNode);
			assert(argBinding);			

			/*// Match existing instances against args
			{
				for (AST::TemplateDeclaration::Instance& instance : declNode->instances)
				{
					vector<TypeRef> paramTypes;
					for (auto litAndSource: instance.literals)
					{
						auto& type = litAndSource.literal->getType();
						paramTypes.push_back(type);
					}

					// Clone arg types for inferences/comparison since multiple templates might match
					//	 on unification, but not on literal comparison
					vector<TypeRef> argTypes;
					for (auto tArg : node->templateArgs)
					{
						tArg->accept(this);
						argTypes.push_back(tArg->getType(this->context));
					}

					if (auto result = createArgumentUnification(argTypes, paramTypes, argBinding))
					{
						result.apply();

						// Create
						if (

					}
				}

				if (!findings.empty())
				{
					// TODO: decide what to do if multiple templates instances apply
					//	does it matter which one we pick?

					// Apply unification
					findings[0].second.apply();
					generatedSource = findings[0].first.astContext.getSymbolSource(declNode->declaration);
				}
			}*/

			// Create new template instance
			SymbolSource* generatedSource = nullptr;		
			{
				auto* currentScope = this->context->getScope(node);
				assert(currentScope);

				const int instanceId = declNode->instances.size();
				vector<AST::TemplateDeclaration::Instance::LiteralAndSource> instanceLiterals;
				string debugName = string("template ") + declNode->declaration->getSymbolName() + 
				string(", instance: ") + std::to_string(instanceId);
				ASTContext instanceContext(debugName);

				// Run declaration/dependency steps on signature, 
				processDeclarations(&instanceContext, declNode->signature, currentScope);
				resolveDependencies(&instanceContext, declNode->signature);

				// Match template arguments and assign expressions to parameters
				for (ArgumentBinding::Param& tArgBinding : argBinding->params)
				{
					assert(tArgBinding.paramIndex < declNode->signature->inParams.size());
					assert(tArgBinding.argIndex < node->templateArgs.size());
					AST::FunctionInParam* param = declNode->signature->inParams[tArgBinding.paramIndex];
					AST::Expression* expr = node->templateArgs[tArgBinding.argIndex];
					instanceContext.addTemplateExpression(param, expr, this->context);
				}

				// Process signature
				ASTProcessor instanceAp(this->econtext, &instanceContext);
				declNode->signature->accept(&instanceAp);

				// Save the resulting literals from processing
				for (AST::FunctionInParam* param : declNode->signature->inParams)
				{
					string name = string("tConst_") + declNode->declaration->getSymbolName() + string("_") + std::to_string(instanceId) + string("_") + param->name;
					shared<IR::Literal> literal = instanceContext.getTemplateLiteral(param);
					assert(literal);
					SymbolSource* paramSource = instanceContext.getSymbolSource(param);
					assert(paramSource);
					instanceLiterals.emplace_back(AST::TemplateDeclaration::Instance::LiteralAndSource { literal, paramSource, name});
				}

				// Type checking has already been done in signature processing
				//	assert on it, to be sure
				assert(unifyTemplateArguments(this->context, &instanceContext, node, declNode->signature, argBinding));

				// Before we proceed with generating the actual template declaration
				//	we can now check if there is already an identical template instance
				//	that we can/should reuse.
				{
					int literalCount = instanceLiterals.size();
					for (AST::TemplateDeclaration::Instance& existingInstance : declNode->instances)
					{
						if (existingInstance.literals.size() != literalCount)
							continue;
						
						bool foundInstance = true;
						for (int i = 0; i < literalCount; ++i)
						{
							auto& literal1 = instanceLiterals[i];
							auto& literal2 = existingInstance.literals[i];

							// Hack: don't compare data for type variables since the same types can have 
							//	different ids
							if (literal1.literal->getType() != literal2.literal->getType() ||
								(!literal1.literal->getType()->isTypeVariable() &&
									literal1.literal->data != literal2.literal->data))
							{
								foundInstance = false;
								break;
							}
						}

						if (foundInstance)
						{
							generatedSource = existingInstance.astContext.getSymbolSource(declNode->declaration);
							assert(generatedSource);
							break;
						}
					}
				}

				// If no source existed, generate a new one
				if (!generatedSource)
				{
					// Signature scope should be a parent scope for the declaration
					auto* signatureScope = instanceContext.getScope(declNode->signature);
					assert(signatureScope);		

					// Process declaration
					processDeclarations(&instanceContext, declNode->declaration, signatureScope);
					resolveDependencies(&instanceContext, declNode->declaration);
					declNode->declaration->accept(&instanceAp);

					generatedSource = instanceContext.getSymbolSource(declNode->declaration);
					assert(generatedSource);

					// Add instance for future usages
					declNode->addInstance(std::move(instanceContext), std::move(instanceLiterals));
				}
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
		if (symbol->firstInitOrder > node->order)
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

		string str = Evaluation::evaluateExpressionAsString(econtext, *context, *node->expr);
		printLine("Parsing eval string: "); 
		printLine(str, 1);
 
		BufferSourceInput bufferInput(str.c_str(), str.size());
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

	void process(pair<AST::Node*, ASTContext*> node)
	{
		ASTProcessor ap(econtext, node.second);
		node.first->accept(&ap);
	}

	ASTProcessor(EvaluationContext& econtext, ASTContext* context)
		: econtext(econtext)
		, context(context)
	{
	}

	vector<std::optional<TypeRef>> m_expectedReturnTypeStack;
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
