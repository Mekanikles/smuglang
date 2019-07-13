#pragma once
#include "core.h"
#include "utils.h"
#include "input.h"
#include "token.h"
#include "scanner.h"
#include "types.h"
#include "symbols.h"
#include "ast.h"

struct SourceInput
{
	virtual std::unique_ptr<std::istream> createStream() const = 0;
};

struct FileSourceInput : SourceInput
{
	FileSourceInput(const string& filePath)
		: filePath(filePath)
	{
	}

	std::unique_ptr<std::istream> createStream() const override
	{
		return std::make_unique<std::ifstream>(filePath);
	}

	const string filePath;
};

struct BufferSourceInput : SourceInput
{
	BufferSourceInput(const char* data, size_t size)
		: data(data), size(size)
	{
	}

	std::unique_ptr<std::istream> createStream() const override
	{
		return std::make_unique<MemoryInputStream>(data, size);
	}

	const char* data;
	size_t size;
};	

struct Parser
{
	struct ParserError
	{
		string msg;
		Token token;
		uint column;
		uint row;
	};

	void pushScanner(Scanner& scanner)
	{
		this->scanners.push_back(&scanner);
	}

	void popScanner()
	{
		this->scanners.pop_back();
	}

	template<typename T>
	struct ScopedScanner
	{
		ScopedScanner(T s, Parser* p) : scanner(s), parser(p) { parser->pushScanner(&scanner); }
		~ScopedScanner() { parser->popScanner(); }

		T scanner;
		Parser* parser;
	};

	Scanner* getScanner()
	{
		return this->scanners.back();
	}

	const Token& lastToken()
	{
		return this->tokens.back();
	}

	void error(const Token& token, uint column, uint row, string msg)
	{
		this->parserErrors.push_back(ParserError { msg, token, column, row });
		this->newErrors++;
	}

	void errorOnAccept(string msg)
	{
		error(lastToken(), this->lastTokenColumn, this->lastTokenRow, msg);
	}

	void errorOnExpect(string msg)
	{
		auto scanner = getScanner();
		error(this->currentToken, scanner->lastTokenColumn(), scanner->lastTokenRow(), msg);
	}

	int getNewErrors() { const int ret = this->newErrors; this->newErrors = 0; return ret; }

	void advanceToken()
	{
		auto scanner = getScanner();
		this->lastTokenColumn = scanner->lastTokenColumn();
		this->lastTokenRow = scanner->lastTokenRow();
		this->tokens.push_back(this->currentToken);

		// Make sure to retrieve a valid token
		while(!scanner->getToken(&this->currentToken)) {};
		//printLine(string("Found token: ") + toString(this->currentToken));
	}

	const Token& peek(TokenType type)
	{
		if (this->currentToken.type == type)
			return this->currentToken;
		return s_invalidToken;
	}

	bool accept(TokenType type)
	{
		//printLine(string("Accept: ") + toString(type) + ((this->currentToken.type != type)?  (string(" (got ") + toString(this->currentToken.type) + ")") : ""));	
		if (peek(type))
		{
			advanceToken();
			return true;
		}
		return false;
	}

	bool expect(TokenType type, bool skipOnError = true)
	{
		//printLine(string("Expect: ") + toString(type) + ((this->currentToken.type != type)? (string(" (got ") + toString(this->currentToken.type) + ")") : ""));
		if (accept(type))
			return true;
		
		string errMsg = 
				string("Expected ") + toString(type) + " but got " + 
				toString(this->currentToken.type);	
		errorOnExpect(errMsg);
		if (skipOnError)
			advanceToken();
		return false;
	}

	bool acceptUnaryOperator()
	{
		if (accept(TokenType::Plus))
		{
 			return true;
		}
		else if (accept(TokenType::Minus))
		{
	 		return true;
		}
		else if (accept(TokenType::Asterisk))
		{
		 	return true;
		}
		else if (accept(TokenType::Ampersand))
		{
		 	return true;
		}					
		else if (accept(TokenType::IncrementOp))
		{
			return true;
		}
		else if (accept(TokenType::DecrementOp))
		{
			return true;
		}

		return false;
	}

	bool acceptUnaryPostfixOperator()
	{
		if (accept(TokenType::IncrementOp))
		{
			return true;
		}
		else if (accept(TokenType::DecrementOp))
		{
			return true;
		}

		return false;
	}

	bool acceptBinaryOperator()
	{
		auto ops = { TokenType::EqualsOp,
					 TokenType::Plus,
					 TokenType::Minus,
					 TokenType::Asterisk,
					 TokenType::Slash,
					 TokenType::Modulo,
					 TokenType::LessThanOp,
					 TokenType::GreaterThanOp,
					 TokenType::LessThanOrEqualsOp,
					 TokenType::GreaterThanOrEqualsOp };

		for (auto& op : ops)
		{
			if (accept(op))
				return true;
		}

		return false;
	}

	bool parseFunctionExpressionContinuation(AST::Expression** outExpr)
	{
		// Optional signature
		AST::FunctionSignature* signature = nullptr;
		if (!parseFunctionSignature(&signature))
		{
			// Default empty signature
			signature = createNode<AST::FunctionSignature>();
		}

		AST::StatementBody* statementBody = nullptr;
		if (parseStatementBody(&statementBody))
		{
			auto* func = createNode<AST::FunctionLiteral>();
			func->signature = signature;
			func->body = statementBody;

			*outExpr = func;
		}
		else
		{
			*outExpr = signature;
		}

		return true;
	}

	bool parseFunctionExpression(AST::Expression** outExpr)
	{
		if (accept(TokenType::Func))
		{
			parseFunctionExpressionContinuation(outExpr);
			return true;
		}
		
		return false;
	}

	bool parseStructExpressionContinuation(AST::Expression** outExpr)
	{
		// TODO: Create struct expression
		assert(false);
		outExpr = nullptr;
		return true;
	}

	bool parseStructExpression(AST::Expression** outExpr)
	{
		if (accept(TokenType::Struct))
		{
			parseStructExpressionContinuation(outExpr);
			return true;
		}
		
		return false;
	}


	bool parseTypeLiteral(AST::TypeLiteral** outNode)
	{
		if (const Token& token = peek(TokenType::CompilerDirective))
		{
			if (token.symbol == "primitives")
			{
				advanceToken();
				
				// TODO: Generalise primitives as list of types
				expect(TokenType::Dot);

				expect(TokenType::Symbol);

				if (lastToken().symbol == "type")
				{
					auto* node = createNode<AST::TypeLiteral>(createTypeVariable());		
					*outNode = node;
					return true;
				}

				PrimitiveClass::SignedType sign = PrimitiveClass::UnknownSign;
				int size = -1; 

				auto type = PrimitiveClass::Int;

				if (lastToken().symbol == "c")
				{
					expect(TokenType::Dot);
					expect(TokenType::Symbol);
					if (lastToken().symbol == "int")
					{
						sign = PrimitiveClass::Signed;
						size = 32;
					}
					else if (lastToken().symbol == "char")
					{
						type = PrimitiveClass::Char;
						sign = PrimitiveClass::Signed;
						size = 8;
					}
					else
					{
						errorOnAccept("Unknown c primitive");
					}
				}
				else if (lastToken().symbol == "int")
				{
					// TODO: Make int a proper parameterized type
					//	with size and sign defaulting to 32, true
					//	This allows user to write "int" for unspecified ints
					if (accept(TokenType::OpenParenthesis))
					{
						expect(TokenType::IntegerLiteral);

						size = atoi(lastToken().symbol.c_str());
						if (size <= 0)
						{
							errorOnAccept("Size must be a positive integer");
						}

						expect(TokenType::Comma);
						if (accept(TokenType::True))
						{
							sign = PrimitiveClass::Signed;
						}
						else if (accept(TokenType::False))
						{
							sign = PrimitiveClass::Unsigned;
						}
						else
						{
							errorOnExpect("Expected boolean");
						}
						
						expect(TokenType::CloseParenthesis);
					}
				}
				else if (lastToken().symbol == "s64")
				{
					sign = PrimitiveClass::Signed;
					size = 64;
				}				
				else if (lastToken().symbol == "u64")
				{
					sign = PrimitiveClass::Unsigned;
					size = 64;
				}
				else if (lastToken().symbol == "s32")
				{
					sign = PrimitiveClass::Signed;
					size = 32;
				}
				else if (lastToken().symbol == "u32")
				{
					sign = PrimitiveClass::Unsigned;
					size = 32;
				}
				else if (lastToken().symbol == "s16")
				{
					sign = PrimitiveClass::Signed;
					size = 16;
				}
				else if (lastToken().symbol == "u16")
				{
					sign = PrimitiveClass::Unsigned;
					size = 16;
				}
				else if (lastToken().symbol == "s8")
				{
					sign = PrimitiveClass::Signed;
					size = 8;
				}
				else if (lastToken().symbol == "u8")
				{
					sign = PrimitiveClass::Unsigned;
					size = 8;
				}
				else if (lastToken().symbol == "f128")
				{
					type = PrimitiveClass::Float;
					size = 128;
				}
				else if (lastToken().symbol == "f64")
				{
					type = PrimitiveClass::Float;
					size = 64;
				}
				else if (lastToken().symbol == "f32")
				{
					type = PrimitiveClass::Float;
					size = 32;
				}
				else if (lastToken().symbol == "f16")
				{
					type = PrimitiveClass::Float;
					size = 16;
				}
				else
				{
					errorOnAccept("Unknown primitive type");
				}

				auto typeClass = (size == -1) ?
					std::make_unique<PrimitiveClass>(type, sign) :
					std::make_unique<PrimitiveClass>(type, (uint)size, sign);
				auto* node = createNode<AST::TypeLiteral>(Type(std::move(typeClass)));		
				*outNode = node;

				return true;
			}
		}

		return false;
	}

	bool parsePrimaryExpression(AST::Expression** outNode)
	{
		AST::TypeLiteral* typeLiteral;
		if (accept(TokenType::OpenParenthesis))
		{
			auto* tuple = createNode<AST::Tuple>();

			AST::Expression* expr = nullptr;
			if (parseExpression(&expr))
			{
				tuple->exprs.push_back(expr);
				while (accept(TokenType::Comma))
				{
					if (parseExpression(&expr))
						tuple->exprs.push_back(expr);
					else
						errorOnExpect("Expected expression");
				}		
			}

			expect(TokenType::CloseParenthesis);

			*outNode = tuple;
		}
		else if (acceptUnaryOperator())
		{
			auto uop = createNode<AST::UnaryOp>();
			uop->opType = lastToken().type;

			AST::Expression* expr = nullptr;
			if (parsePrimaryExpression(&expr))
			{
				uop->expr = expr;
				*outNode = uop;
			}
			else
			{
				errorOnExpect("Expected primary expression");
				*outNode = nullptr;
				return true;
			}
		}
		else if (accept(TokenType::StringLiteral))
		{
			auto* expr = createNode<AST::StringLiteral>(lastToken().symbol);
			*outNode = expr;
		}
		else if (accept(TokenType::IntegerLiteral))
		{
			auto* expr = createNode<AST::IntegerLiteral>(lastToken().symbol, AST::IntegerLiteral::Decimal);
			*outNode = expr;
		}
		else if (accept(TokenType::HexadecimalLiteral))
		{
			auto* expr = createNode<AST::IntegerLiteral>(lastToken().symbol, AST::IntegerLiteral::Hexadecimal);
			*outNode = expr;
		}
		else if (accept(TokenType::BinaryLiteral))
		{
			auto* expr = createNode<AST::IntegerLiteral>(lastToken().symbol, AST::IntegerLiteral::Binary);
			*outNode = expr;
		}				
		else if (accept(TokenType::FloatLiteral))
		{
			auto* expr = createNode<AST::FloatLiteral>(lastToken().symbol);
			*outNode = expr;
		}
		else if (accept(TokenType::Symbol))
		{
			string symbol = lastToken().symbol;

			auto* expr = createNode<AST::SymbolExpression>(symbol);

			// Template parameters
			if (accept(TokenType::TemplateParameterOpener))
			{
				if (!parseTemplateArguments(expr))
				{
					errorOnExpect("Template parameter list expected");
				}

				expect(TokenType::CloseParenthesis);
			}

			// TODO: Support any primary expression as a call initializer
			if (accept(TokenType::OpenParenthesis))
			{	
				auto node = createNode<AST::Call>();
				node->expr = expr;

				if (!parseCallArguments(node))
				{
					errorOnExpect("Call arguments expected");
				}

				expect(TokenType::CloseParenthesis);
				*outNode = node;	
			}
			else
			{
				*outNode = expr;	
			}
		}
		else if (accept(TokenType::Func))
		{
			// Optional signature
			AST::FunctionSignature* signature;
			if (!parseFunctionSignature(&signature))
			{
				// Lone func is shorthand for func() -> ();
				signature = createNode<AST::FunctionSignature>();
			}

			*outNode = signature;
		}
		else if (parseTypeLiteral(&typeLiteral))
		{
			*outNode = typeLiteral;
		}
		else if (parseFunctionExpression(outNode))
		{
		}
		else if (parseStructExpression(outNode))
		{
		}
		else
		{
			return false;
		}

		// Accept any expression post-modifiers
		while(true)
		{
			if (acceptUnaryPostfixOperator())
			{
				// Postfix ops
				auto uop = createNode<AST::UnaryPostfixOp>();
				uop->opType = lastToken().type;
				uop->expr = *outNode;

				*outNode = uop;
			}
			else if (accept(TokenType::Dot))
			{
				// Member access
				auto ma = createNode<AST::MemberAccess>();
				ma->expr = *outNode;
				AST::SymbolExpression* symExpr = nullptr;
				if (!parseSymbolExpression(&symExpr))
					errorOnExpect("Expected symbol expression");
				ma->member = symExpr;

				*outNode = ma;
			}
			else if (accept(TokenType::OpenParenthesis))
			{	
				// Function call
				auto call = createNode<AST::Call>();
				call->expr = *outNode;

				if (!parseCallArguments(call))
				{
					errorOnExpect("Call parameter list expected");
				}

				expect(TokenType::CloseParenthesis);

				*outNode = call;
			}
			else
			{
				break;
			}
		}

		return true;
	}

	int operatorPrecedence(TokenType opType)
	{
		switch (opType)
		{
			case TokenType::Asterisk:
			case TokenType::Slash:
			case TokenType::Modulo:
				return 10;
			case TokenType::Plus:
			case TokenType::Minus:
				return 9;
			case TokenType::EqualsOp:
			case TokenType::LessThanOp:
			case TokenType::GreaterThanOp:
			case TokenType::LessThanOrEqualsOp:
			case TokenType::GreaterThanOrEqualsOp:
				return 1;
			default:
				return 0;
		}
	}

	bool hasHigherOperatorPrecedence(TokenType left, TokenType right)
	{
		const int l = operatorPrecedence(left);
		const int r = operatorPrecedence(right);

		// TODO: Deal with right-associative operators
		return (r > l);
	}

	bool parseExpression(AST::Expression** outExpr)
	{
		// Shunt all the yards!
		AST::Expression* exprNode = nullptr;
		if (!parsePrimaryExpression(&exprNode))
			return false;

		vector<TokenType> binOpStack;
		vector<AST::Expression*> exprStack;

		exprStack.push_back(exprNode);

		while (acceptBinaryOperator())
		{
			const auto op = lastToken().type;

			// Resolve as many previous ops as possible
			while (binOpStack.size() > 0 && !hasHigherOperatorPrecedence(binOpStack.back(), op))
			{
				assert(exprStack.size() > 1);
				auto* bop = createNode<AST::BinaryOp>();
				bop->opType = binOpStack.back();
				binOpStack.pop_back();
				bop->right = exprStack.back();
				exprStack.pop_back();
				bop->left = exprStack.back();

				exprStack.back() = bop;
			}

			binOpStack.push_back(op);

			// To allow some operators to both work as binary and unary postfix
			//	we try to bind to a primary expression first
			if (parsePrimaryExpression(&exprNode))
			{
				exprStack.push_back(exprNode);
			}
			else if (binOpStack.back() == TokenType::Asterisk || 
					binOpStack.back() == TokenType::Ampersand)
			{
				// If we cannot find an right hand expression, we interpret
				//	 the operator as unary postfix
				assert(!exprStack.empty());
				auto uop = createNode<AST::UnaryPostfixOp>();
				uop->opType = binOpStack.back();
				binOpStack.pop_back();
				uop->expr = exprStack.back();

				exprStack.back() = uop;
			}
			else
			{
				errorOnExpect("Expected primary expression");
				*outExpr = nullptr;
				return true;
			}
		}

		// Take care of remaining ops
		while (binOpStack.size() > 0)
		{
			auto* bop = createNode<AST::BinaryOp>();
			bop->opType = binOpStack.back();
			binOpStack.pop_back();
			bop->right = exprStack.back();
			exprStack.pop_back();
			bop->left = exprStack.back();
			exprStack.back() = bop;	
		}

		assert(exprStack.size() == 1);
		*outExpr = exprStack.back();

		return true;
	}

	bool parseCallArguments(AST::Call* call)
	{
		AST::Expression* exprNode;
		if (parseExpression(&exprNode))
		{
			call->args.push_back(exprNode);

			while (accept(TokenType::Comma))
			{
				if (parseExpression(&exprNode))
					call->args.push_back(exprNode);
				else
					errorOnExpect("Expected expression");
			}
		}

		return true;
	}

	// TODO: generalize argument lists
	bool parseTemplateArguments(AST::SymbolExpression* expr)
	{
		AST::Expression* exprNode;
		if (parseExpression(&exprNode))
		{
			expr->templateArgs.push_back(exprNode);

			while (accept(TokenType::Comma))
			{
				if (parseExpression(&exprNode))
					expr->templateArgs.push_back(exprNode);
				else
					errorOnExpect("Expected expression");
			}
		}

		return true;
	}

	string stringSymbolValue(string symbol)
	{
		return symbol.substr(1, symbol.length() - 2);
	}


	void skipToNextStatement()
	{
		// TODO: Can this be generalized for non semi-colon statements?
		// If we encountered errors and could not parse anymore statements, try to jump to next statement
		while (!accept(TokenType::SemiColon))
		{
			advanceToken();
			if (peek(TokenType::EndOfScan))
				return;
		}
	}

	bool parseSymbolDeclaration(AST::SymbolDeclaration** outDeclaration, StorageQualifier storageQualifier)
	{
		if (!accept(TokenType::Symbol))
			return false;
		
		auto* node = createNode<AST::SymbolDeclaration>();
		*outDeclaration = node;

		node->symbol = lastToken().symbol;
		node->storageQualifier = storageQualifier;

		// Optional type declaration
		if (accept(TokenType::Colon))
		{
			// TODO: Should handle generic expressions
			AST::Expression* expr;
			if (!parseExpression(&expr))
			{
				errorOnExpect("Expected expression");
			}

			node->typeExpr = expr;
		}

		// Optional initialization
		// TODO: Not allowed for externals, add custom error
		if (!node->isExternal() && accept(TokenType::Equals))
		{
			AST::Expression* expr;
			if (parseExpression(&expr))
			{
				node->initExpr = expr;
			}
			else
			{
				errorOnExpect("Expected expression");
			}
		}

		return true;
	}

	bool parseFunctionInParam(AST::FunctionInParam** outNode)
	{
		if (!accept(TokenType::Symbol))
			return false;
		
		auto node = createNode<AST::FunctionInParam>();
		node->name = lastToken().symbol;

		if (accept(TokenType::Ellipsis))
			node->isVariadic = true;

		// Optional type declaration
		node->typeExpr = nullptr;
		if (accept(TokenType::Colon))
		{
			// TODO: Should handle generic expressions
			AST::Expression* expr;
			if (!parseExpression(&expr))
			{
				errorOnExpect("Expected expression");
			}

			node->typeExpr = expr;
		}

		// Optional initialization
		node->initExpr = nullptr;
		if (accept(TokenType::Equals))
		{
			if (node->isVariadic)
				errorOnAccept("Cannot have default arguments for variadic paramters");

			AST::Expression* expr;
			if (parseExpression(&expr))
			{
				node->initExpr = expr;
			}
			else
			{
				errorOnExpect("Expected expression");
			}
		}

		*outNode = node;

		return true;
	}

	bool parseFunctionOutParam(AST::FunctionOutParam** outNode)
	{
		AST::FunctionOutParam* node = nullptr;
		AST::Expression* firstExpr = nullptr;

		if (parseExpression(&firstExpr))
		{
			node = createNode<AST::FunctionOutParam>();

			// If we have optional type declaration, first expression must be param name
			if (accept(TokenType::Colon))
			{
				if (!firstExpr->isSymbolExpression())
				{
					errorOnAccept("Parameter name before colon must be simple symbol");
				}

				// TODO: Here we leave firstExpr object dangling. Problem?
				auto* symExpr = static_cast<AST::SymbolExpression*>(firstExpr);
				node->name = symExpr->symbol;

				if (!parseExpression(&node->typeExpr))
				{
					errorOnExpect("Expected type expression");
				}
			}
			else
			{
				node->typeExpr = firstExpr;
			}
		}

		if (!node)
			return false;

		*outNode = node;
		return true;
	}

	bool parseFunctionSignature(AST::FunctionSignature** outSignature, bool forceDefQualifier = false)
	{
		AST::FunctionSignature* node;

		if (accept(TokenType::OpenParenthesis))
		{
			node = createNode<AST::FunctionSignature>();
			*outSignature = node;

			node->specifiedInParams = true;

			// Inparams
			AST::FunctionInParam* inParam;
			if (parseFunctionInParam(&inParam))
			{
				node->inParams.push_back(inParam);

				if (forceDefQualifier)
					inParam->storageQualifier = StorageQualifier::Def;

				while(accept(TokenType::Comma))
				{
					if (parseFunctionInParam(&inParam))
					{
						node->inParams.push_back(inParam);
					}
					else
					{
						errorOnExpect("Expected parameter");
						break;
					}
				}
			}
			expect(TokenType::CloseParenthesis);
		}

		// Output params
		AST::FunctionOutParam* outParam;
		if (accept(TokenType::Arrow))
		{
			if (!node)
				node = createNode<AST::FunctionSignature>();

			if (accept(TokenType::OpenParenthesis))
			{
				while (parseFunctionOutParam(&outParam))
				{
					node->outParams.push_back(outParam);

					if (accept(TokenType::Comma))
					{
						continue;
					}
					else
					{
						break;
					}
				}
				expect(TokenType::CloseParenthesis);
			}
			else if (parseFunctionOutParam(&outParam))
			{
				node->outParams.push_back(outParam);
			}
			else
			{
				errorOnExpect("Expected out parameter declaration");
			}
		}

		if (!node)
			return false;

		*outSignature = node;
		return true;
	}

	bool parseStatementBody(AST::StatementBody** outStatementBody)
	{
		if (accept(TokenType::OpenBrace))
		{
			auto* node = createNode<AST::StatementBody>();
			*outStatementBody = node;

			AST::Statement* statement;
			while(parseStatement(&statement))
			{
				node->addStatement(statement);
			}

			expect(TokenType::CloseBrace);

			return true;
		}
		return false;
	}

	void parseFunctionDeclarationContinuation(AST::FunctionDeclaration** outDeclaration)
	{
		if (expect(TokenType::Symbol))
		{
			const string functionName = lastToken().symbol;
			*outDeclaration = nullptr;
			
			AST::FunctionSignature* signature;
			if (!parseFunctionSignature(&signature))
			{
				errorOnAccept("Expected function signature following function declaration");
				return;
			}
			
			AST::StatementBody* statementBody;
			if (!parseStatementBody(&statementBody))
			{
				errorOnAccept("Expected statement body following function declaration");
				return;
			}

			auto node = createNode<AST::FunctionDeclaration>();
			node->symbol = functionName;

			auto* func = createNode<AST::FunctionLiteral>();
			func->signature = signature;
			func->body = statementBody;

			node->funcLiteral = func;

			*outDeclaration = node;
		}
	}

	bool parseFunctionDeclaration(AST::FunctionDeclaration** outDeclaration)
	{
		if (accept(TokenType::Func))
		{
			parseFunctionDeclarationContinuation(outDeclaration);
			return true;
		}

		return false;
	}

	bool parseFunctionDeclarationOrExpression(AST::Statement** outStatement)
	{
		if (accept(TokenType::Func))
		{
			*outStatement = nullptr;
			// Short-hand func declaration
			if (peek(TokenType::Symbol))
			{
				AST::FunctionDeclaration* funcDecl;
				parseFunctionDeclarationContinuation(&funcDecl);
				*outStatement = funcDecl;
			}
			else
			{
				AST::Expression* expr;
				parseFunctionExpressionContinuation(&expr);
				*outStatement = expr;
			}
			return true;
		}

		return false;
	}

	void parseStructDeclarationContinuation(AST::StructDeclaration** outDeclaration)
	{
		if (expect(TokenType::Symbol))
		{
			const string structName = lastToken().symbol;
			*outDeclaration = nullptr;

			auto node = createNode<AST::StructDeclaration>();
			node->name = structName;

			if (accept(TokenType::OpenBrace))
			{
				while(true)
				{
					if (accept(TokenType::Symbol))
					{
						const string fieldName = lastToken().symbol;
						expect(TokenType::Colon);
						
						// Type expression
						AST::Expression* typeExpr = nullptr;
						if (!parseExpression(&typeExpr))
						{
							errorOnExpect("Expected type expression");
						}

						// Optional initialization
						AST::Expression* initExpr = nullptr;
						if (accept(TokenType::Equals))
						{
							AST::Expression* expr = nullptr;
							if (!parseExpression(&expr))
							{
								errorOnExpect("Expected expression");
							}
						}

						auto field = createNode<AST::StructField>();
						field->name = fieldName;
						field->typeExpr = typeExpr;
						field->initExpr = initExpr;
						node->fields.push_back(field);

						expect(TokenType::SemiColon);

						continue;
					}

					break;
				}

				expect(TokenType::CloseBrace);
			}

			*outDeclaration = node;
		}
	}

	bool parseStructDeclaration(AST::StructDeclaration** outDeclaration)
	{
		if (accept(TokenType::Struct))
		{
			parseStructDeclarationContinuation(outDeclaration);
			return true;
		}

		return false;
	}

	bool parseStructDeclarationOrExpression(AST::Statement** outStatement)
	{
		if (accept(TokenType::Struct))
		{
			*outStatement = nullptr;
			// Short-hand func declaration
			if (peek(TokenType::Symbol))
			{
				AST::StructDeclaration* structDecl;
				parseStructDeclarationContinuation(&structDecl);
				*outStatement = structDecl;
			}
			else
			{
				AST::Expression* expr;
				parseStructExpressionContinuation(&expr);
				*outStatement = expr;
			}
			return true;
		}

		return false;
	}

	// TODO: Should output statement?
	bool parseDeclarationStatement(AST::Declaration** outDeclaration)
	{
		AST::FunctionDeclaration* funcDecl;
		AST::StructDeclaration* structDecl;
		if (accept(TokenType::Var) || accept(TokenType::Def) || accept(TokenType::Const) || accept(TokenType::Extern))
		{
			StorageQualifier sq;
			if (lastToken().type == TokenType::Var)
				sq = StorageQualifier::Var;
			else if (lastToken().type == TokenType::Const)
				sq = StorageQualifier::Const;
			else if (lastToken().type == TokenType::Def)
				sq = StorageQualifier::Def;
			else
				sq = StorageQualifier::Extern;

			AST::SymbolDeclaration* symbolDecl;
			if (!parseSymbolDeclaration(&symbolDecl, sq))
			{
				errorOnExpect("Expected symbol declaration");
				*outDeclaration = nullptr;
				return true;
			}

			// TODO: Set storage qualifier
			*outDeclaration = symbolDecl;
			return true;
		}
		else if (parseFunctionDeclaration(&funcDecl))
		{
			*outDeclaration = funcDecl;
			return true;
		}
		else if (parseStructDeclaration(&structDecl))
		{
			*outDeclaration = structDecl;
			return true;
		}

		return false;
	}

	bool parseTemplatedDeclarationStatement(AST::TemplateDeclaration** outTemplateDeclaration)
	{
		if (const Token& token = peek(TokenType::CompilerDirective))
		{
			if (token.symbol == "template")
			{
				advanceToken();

				AST::TemplateDeclaration* node = createNode<AST::TemplateDeclaration>();

				if (!parseFunctionSignature(&node->signature, true))
				{
					errorOnAccept("Expected signature following template declaration");
					return true;
				}

				AST::Declaration* innerDecl = nullptr;
				if (!parseDeclarationStatement(&innerDecl))
				{
					errorOnAccept("Expected declaration statement");
				}

				node->declaration = innerDecl;
				*outTemplateDeclaration = node;

				return true;
			}
		}

		return false;
	}

	bool parseIfStatement(AST::IfStatement** outStatement)
	{
		if (accept(TokenType::If))
		{
			expect(TokenType::OpenParenthesis);	
			AST::IfStatement* node = createNode<AST::IfStatement>();

			AST::Expression* expr;
			if (parseExpression(&expr))
			{
				node->expr = expr;
			}
			else
			{
				errorOnExpect("Expected expression");
			}

			expect(TokenType::CloseParenthesis);	

			// Body
			AST::Statement* statement;
			if (parseStatement(&statement))
			{
				node->statement = statement;
			}
			else
			{
				errorOnExpect("Expected statement");
			}

			if (accept(TokenType::Else))
			{
				AST::Statement* statement;
				if (parseStatement(&statement))
				{
					node->elseStatement = statement;
				}
				else
				{
					errorOnExpect("Expected statement");
				}
			}

			*outStatement = node;

			return true;
		}

		return false;
	}

	bool parseEvalStatement(AST::EvalStatement** outStatement)
	{
		if (accept(TokenType::Eval))
		{
			expect(TokenType::OpenParenthesis);	
			AST::EvalStatement* node = createNode<AST::EvalStatement>();
			
			AST::Expression* expr;
			if (parseExpression(&expr))
			{
				node->expr = expr;
			}
			else
			{
				errorOnExpect("Expected expression");
			}

			expect(TokenType::CloseParenthesis);

			*outStatement = node;

			return true;
		}

		return false;
	}

	bool parseSymbolExpression(AST::SymbolExpression** node)
	{
		if (accept(TokenType::Symbol))
		{
			Token t = lastToken();
			string symbol = t.symbol;

			*node = createNode<AST::SymbolExpression>(symbol);

			// Template parameters
			if (accept(TokenType::TemplateParameterOpener))
			{
				if (!parseTemplateArguments(*node))
				{
					errorOnExpect("Template parameter list expected");
				}

				expect(TokenType::CloseParenthesis);
			}

			return true;
		}

		return false;
	}

	bool parseLoopStatement(AST::LoopStatement** outStatement)
	{
		if (accept(TokenType::Loop))
		{
			AST::Statement* statement = nullptr;
			if (!parseStatement(&statement))
			{
				errorOnExpect("Expected Statement");
			}

			auto node = createNode<AST::LoopStatement>();
			node->statement = statement;

			*outStatement = node;

			return true;
		}

		return false;
	}

	bool parseStatement(AST::Statement** outStatement)
	{
		AST::Declaration* declaration;
		AST::TemplateDeclaration* templateDeclaration;
		AST::IfStatement* ifStatement;
		AST::EvalStatement* evalStatement;
		AST::StatementBody* statementBody;
		AST::Expression* expression;
		AST::LoopStatement* loopStatement;

		if (accept(TokenType::Import))
		{
			AST::Import::LinkType linkType = AST::Import::LinkType_Native;

			// optional import params
			// TODO: invoke new parameter list scanner?
			if (accept(TokenType::OpenParenthesis))
			{
				expect(TokenType::StringLiteral);	
				if (stringSymbolValue(lastToken().symbol) != "c")
					errorOnAccept("Only 'c' imports type specifier is supported");

				linkType = AST::Import::LinkType_C;

				expect(TokenType::CloseParenthesis);	
			}
		
			expect(TokenType::StringLiteral);
			const string file = stringSymbolValue(lastToken().symbol);

			expect(TokenType::SemiColon);

			auto node = createNode<AST::Import>();
			node->file = file;
			if(linkType != AST::Import::LinkType_C)
				errorOnAccept("Only 'c' type imports are supported");
			node->linkType = linkType;
			*outStatement = node;
		}
		if (accept(TokenType::Defer))
		{
			auto* node = createNode<AST::DeferStatement>();
			AST::Statement* deferStatement;
			if (!parseStatement(&deferStatement))
			{
				errorOnExpect("Expected Statement");
			}

			node->statement = deferStatement;

			*outStatement = node;
		}
		else if (accept(TokenType::Return))
		{
			auto* node = createNode<AST::ReturnStatement>();

			// Optional expression
			AST::Expression* expr;
			if (parseExpression(&expr))
			{
				node->expr = expr;
			}

			expect(TokenType::SemiColon, false);
			*outStatement = node;
		}
		else if (parseLoopStatement(&loopStatement))
		{
			expect(TokenType::SemiColon, false);
			*outStatement = loopStatement;
		}
		else if (accept(TokenType::Continue))
		{
			auto* node = createNode<AST::ContinueStatement>();
			// TODO: Optional label
			expect(TokenType::SemiColon, false);
			*outStatement = node;
		}
		else if (accept(TokenType::Break))
		{
			auto* node = createNode<AST::BreakStatement>();
			// TODO: Optional label
			expect(TokenType::SemiColon, false);
			*outStatement = node;
		}
		else if (parseFunctionDeclarationOrExpression(outStatement)) // Check this before expression!
		{
			expect(TokenType::SemiColon, false);
		}
		else if (parseStructDeclarationOrExpression(outStatement))
		{
			expect(TokenType::SemiColon, false);
		}
		else if (parseExpression(&expression))
		{
			if (accept(TokenType::Equals))
			{
				auto* assignment = createNode<AST::Assignment>();
				*outStatement = assignment;

				// Assignment
				AST::Expression* assignExpr;
				if (!parseExpression(&assignExpr))
				{
					errorOnExpect("Expected expression");
				}

				assignment->target = expression;
				assignment->expr = assignExpr;

				*outStatement = assignment;
			}
			else
			{
				*outStatement = expression;
			}

			expect(TokenType::SemiColon);
		}		
		else if (parseTemplatedDeclarationStatement(&templateDeclaration))
		{
			// TODO: Should declarations ending in bodies require semi colon?
			expect(TokenType::SemiColon, false);			
			*outStatement = templateDeclaration;
		}
		else if (parseDeclarationStatement(&declaration))
		{
			// TODO: Should declarations ending in bodies require semi colon?
			expect(TokenType::SemiColon, false);
			*outStatement = declaration;
		}
		else if (parseIfStatement(&ifStatement))
		{
			*outStatement = ifStatement;
		}
		else if (parseEvalStatement(&evalStatement))
		{
			expect(TokenType::SemiColon, false);
			*outStatement = evalStatement;
		}
		else if (parseStatementBody(&statementBody))
		{
			*outStatement = statementBody;
		}
		else
		{
			return false;
		}

		return true;
	}

	// TODO: Rename parseModule
	bool parseModule(AST::Module* module)
	{
		auto* body = createNode<AST::StatementBody>();
		module->body = body;

		while(!peek(TokenType::EndOfScan))
		{
			AST::Statement* stmnt;
			if (parseStatement(&stmnt))
			{
				body->addStatement(stmnt);
			}
			else
			{
				errorOnExpect("Expected statement");
				skipToNextStatement();
			}
		}

		return false;
	}

	bool parse(AST::ASTObject* ast)
	{
		assert(!ast->module);		
		auto* node = createNode<AST::Module>();
	
		parseModule(node);
		expect(TokenType::EndOfScan);

		ast->module = node;
		return getParserErrors().size() == 0 && getScannerErrors().size() == 0;
	}

	const vector<ParserError>& getParserErrors() const { return this->parserErrors; }
	const vector<ScannerError>& getScannerErrors() const { return this->scannerFactory.getScannerErrors(); }
	const vector<Token> getTokens() const { return this->tokens; }
	const SourceInput* getSourceInput() const { return this->sourceInput; }

	Parser(const SourceInput* sourceInput)
		: scannerFactory(BufferedInputStream(sourceInput->createStream()))
		, baseScanner(scannerFactory.scanTopLevel())
		, sourceInput(sourceInput)
	{
		pushScanner(this->baseScanner);
		advanceToken();
	}

	ScannerFactory scannerFactory;
	TopLevelScanner baseScanner;
	const SourceInput* sourceInput;

	vector<Token> tokens;
	Token currentToken = Token(TokenType::StartOfScan);
	vector<Scanner*> scanners;
	vector<ParserError> parserErrors;
	int newErrors = 0;
	uint lastTokenColumn = 0;
	uint lastTokenRow = 0;
};









