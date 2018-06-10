#pragma once

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

	bool peek(TokenType type)
	{
		return this->currentToken.type == type;
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
		if (accept(TokenType::CompareOp))
		{
			return true;
		}
		else if (accept(TokenType::Plus))
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
		else if (accept(TokenType::Slash))
		{
			 return true;
		}		
		return false;
	}

	bool parseFunctionExpression(AST::Expression** outExpr)
	{
		if (accept(TokenType::Func))
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
		
		return false;
	}

	bool parseTypeLiteral(AST::TypeLiteral** outNode)
	{
		if (accept(TokenType::CompilerDirective))
		{
			if (lastToken().symbol == "primitives")
			{
				// TODO: Generalise primitives as list of types
				expect(TokenType::Dot);

				expect(TokenType::Symbol);
				PrimitiveClass::SignedType sign = PrimitiveClass::UnknownSign;
				uint size;

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
				else
				{
					errorOnAccept("Unknown primitive type");
				}

				auto typeClass = std::make_unique<PrimitiveClass>(type, size, sign);
				auto* node = createNode<AST::TypeLiteral>(std::move(typeClass));		
				*outNode = node;
			}
			else
			{
				errorOnAccept("Unknown compiler directive");
			}

			return true;
		}

		return false;
	}

	bool parsePrimaryExpressionSymbolContinuation(const string& symbolName, AST::Expression** outNode)
	{
		// TODO: Parse function call etc
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
			auto* expr = createNode<AST::IntegerLiteral>(lastToken().symbol);
			*outNode = expr;
		}
		else if (accept(TokenType::FloatLiteral))
		{
			auto* expr = createNode<AST::FloatLiteral>(lastToken().symbol);
			*outNode = expr;
		}
		else if (accept(TokenType::Symbol))
		{
			if (!parsePrimaryExpressionSymbolContinuation(lastToken().symbol, outNode))
			{
				// Was simple symbol expression
				auto* expr = createNode<AST::SymbolExpression>(lastToken().symbol);
				*outNode = expr;
			}
		}
		else if (parseTypeLiteral(&typeLiteral))
		{
			*outNode = typeLiteral;
		}
		else if (parseFunctionExpression(outNode))
		{
		}
		else
		{
			return false;
		}

		if (acceptUnaryPostfixOperator())
		{
			auto uop = createNode<AST::UnaryPostfixOp>();
			uop->opType = lastToken().type;
			uop->expr = *outNode;

			*outNode = uop;
		}

		return true;
	}

	int operatorPrecedence(TokenType opType)
	{
		switch (opType)
		{
			case TokenType::Asterisk:
			case TokenType::Slash:
				return 10;
			case TokenType::Plus:
			case TokenType::Minus:
				return 9;
			case TokenType::CompareOp:
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

	bool parseCallParameters(AST::Call* call)
	{
		AST::Expression* exprNode;
		while (parseExpression(&exprNode))
		{
			call->args.push_back(exprNode);

			if (peek(TokenType::CloseParenthesis))
			{
				break;
			}
			else if (expect(TokenType::Comma))
			{
				continue;
			}
			else
			{
				break;
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

	bool parseSymbolDeclaration(AST::SymbolDeclaration** outDeclaration, bool isExternal = false)
	{
		if (!accept(TokenType::Symbol))
			return false;
		
		auto* node = createNode<AST::SymbolDeclaration>();
		*outDeclaration = node;

		node->symbol = lastToken().symbol;
		node->isExternal = isExternal;

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
		if (!isExternal && accept(TokenType::Equals))
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
		// See if this is a named output
		if (accept(TokenType::Symbol))
		{
			node = createNode<AST::FunctionOutParam>();

			if (!parsePrimaryExpressionSymbolContinuation(lastToken().symbol, &firstExpr))
			{
				// If we have optional type declaration, we know that symbol was param name
				if (accept(TokenType::Colon))
				{
					node->name = lastToken().symbol;
					if (!parseExpression(&node->typeExpr))
					{
						errorOnExpect("Expected expression");
					}
				}
				else
				{
					node->typeExpr = createNode<AST::SymbolExpression>(lastToken().symbol);
				}
			}
			else
			{
				// If we have a non-simple expression followed by a colon, the param is malformed
				if (peek(TokenType::Colon))
				{
					errorOnAccept("Parameter name must be simple symbol");
				}

				node->typeExpr = firstExpr;
			}
		}
		// Otherwise it is just a type expr
		else if (parseExpression(&firstExpr))
		{
			node = createNode<AST::FunctionOutParam>();

			// If we have a non-simple expression followed by a colon, the param is malformed
			if (peek(TokenType::Colon))
			{
				errorOnAccept("Parameter name must be simple symbol");
			} 

			node->typeExpr = firstExpr;
		}

		if (!node)
			return false;

		*outNode = node;
		return true;
	}

	bool parseFunctionSignature(AST::FunctionSignature** outSignature)
	{
		AST::FunctionSignature* node;

		if (accept(TokenType::OpenParenthesis))
		{
			node = createNode<AST::FunctionSignature>();
			*outSignature = node;

			node->specifiedInParams = true;

			// Inparams
			AST::FunctionInParam* inParam;
			while (parseFunctionInParam(&inParam))
			{
				node->inParams.push_back(inParam);

				if (accept(TokenType::Comma))
				{
					if (accept(TokenType::Ellipsis))
					{
						if (accept(TokenType::Comma))
							continue;
					}
					else
					{
						continue;
					}
				}
				else
				{
					break;
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
			node->scope.parentScope = this->currentScope;
			this->currentScope = &node->scope;

			*outStatementBody = node;

			AST::Statement* statement;
			while(parseStatement(&statement))
			{
				node->addStatement(statement);
			}

			expect(TokenType::CloseBrace);

			this->currentScope = node->scope.parentScope;

			return true;
		}
		return false;
	}

	/*bool parseFunctionDeclaration(AST::FunctionDeclaration** outDeclaration)
	{
		if (accept(TokenType::Func))
		{
			auto* node = createNode<AST::FunctionDeclaration>();
			*outDeclaration = node;

			if (!expect(TokenType::Symbol))
				return true;

			node->symbol = lastToken().symbol;

			AST::FuncLiteralSignature* signature;
			if (!parseFuncLiteralSignature(&signature))
			{
				// TODO: Require parameter list?
				errorOnExpect("Expected function parameter list");
				*outDeclaration = nullptr;
				return true;
			}

			AST::StatementBody* statementBody = nullptr;
			if (!parseStatementBody(&statementBody))
			{
				errorOnExpect("Expected statement body");
			}

			auto* func = createNode<AST::FunctionLiteral>();
			func->signature = signature;
			func->body = statementBody;

			node->funcLiteral = func;

			*outDeclaration = node;
			return true;
		}

		return false;
	}*/

	// TODO: Should output statement?
	bool parseDeclarationStatement(AST::Declaration** outDeclaration)
	{
		//AST::FunctionDeclaration* funcDecl;
		if (accept(TokenType::Var) || accept(TokenType::Def) || accept(TokenType::Extern))
		{
			const bool isExternalDecl = lastToken().type == TokenType::Extern;

			AST::SymbolDeclaration* symbolDecl;
			if (!parseSymbolDeclaration(&symbolDecl, isExternalDecl))
			{
				errorOnExpect("Expected symbol declaration");
				*outDeclaration = nullptr;
				return true;
			}

			// TODO: Set storage qualifier
			*outDeclaration = symbolDecl;
			return true;
		}
		/*else if (parseFunctionDeclaration(&funcDecl))
		{

			*outDeclaration = funcDecl;
			return true;
		}*/

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

	bool parseStatement(AST::Statement** outStatement)
	{
		AST::Declaration* declaration;
		AST::IfStatement* ifStatement;
		AST::EvalStatement* evalStatement;
		AST::StatementBody* statementBody;
		AST::Expression* expression;

		if (accept(TokenType::Func))
		{
			// Short-hand func declaration
			if (accept(TokenType::Symbol))
			{
				const string functionName = lastToken().symbol;
				*outStatement = nullptr;
				AST::FunctionSignature* signature;
				if (!parseFunctionSignature(&signature))
				{
					errorOnAccept("Expected function signature following function delcaration");
					return true;
				}
				
				if (!parseStatementBody(&statementBody))
				{
					errorOnAccept("Expected statement body following function declaration");
					return true;
				}

				auto node = createNode<AST::FunctionDeclaration>();
				node->symbol = functionName;

				auto* func = createNode<AST::FunctionLiteral>();
				func->signature = signature;
				func->body = statementBody;

				node->funcLiteral = func;

				*outStatement = node;
			}
			else
			{
				// Treat this as an expression

				// Optional signature
				AST::FunctionSignature* signature;
				if (!parseFunctionSignature(&signature))
				{
					signature = createNode<AST::FunctionSignature>();
				}

				*outStatement = signature;
			}

			expect(TokenType::SemiColon, false);
		}
		else if (accept(TokenType::Import))
		{
			AST::Import::Type importType = AST::Import::Type_Native;

			// optional import params
			// TODO: invoke new parameter list scanner?
			if (accept(TokenType::OpenParenthesis))
			{
				expect(TokenType::StringLiteral);	
				if (stringSymbolValue(lastToken().symbol) != "c")
					errorOnAccept("Only 'c' imports type specifier is supported");

				importType = AST::Import::Type_C;

				expect(TokenType::CloseParenthesis);	
			}
		
			expect(TokenType::StringLiteral);
			const string file = stringSymbolValue(lastToken().symbol);

			expect(TokenType::SemiColon);

			auto node = createNode<AST::Import>();
			node->file = file;
			if(importType != AST::Import::Type_C)
				errorOnAccept("Only 'c' type imports are supported");
			node->type = importType;
			*outStatement = node;
		}
		// TODO: Parse expression properly
		else if (accept(TokenType::Symbol))
		{
			Token t = lastToken();
			string symbol = t.symbol;

			// TODO: accept expression instead
			//	Will allow assigning to function return values
			// Function call
			if (accept(TokenType::OpenParenthesis))
			{	
				auto node = createNode<AST::Call>();
				node->function = symbol;

				auto* expr = createNode<AST::SymbolExpression>(symbol);
				node->expr = expr;

				if (!parseCallParameters(node))
				{
					errorOnExpect("Call parameter list expected");
				}

				if (!getNewErrors())
				{
					expect(TokenType::CloseParenthesis);
					expect(TokenType::SemiColon);

					*outStatement = node;
				}
				else
				{
					skipToNextStatement();
				}		
			}
			else if (accept(TokenType::Equals))
			{
				auto* node = createNode<AST::Assignment>();
				*outStatement = node;
				
				auto* symExpr = createNode<AST::SymbolExpression>(symbol);

				// Assignment
				AST::Expression* expr;
				if (!parseExpression(&expr))
				{
					errorOnExpect("Expected expression");
				}

				node->symExpr = symExpr;
				node->expr = expr;

				expect(TokenType::SemiColon, false);
			}
			else
			{
				errorOnAccept("Cannot parse lone symbol");
			}
		}
		else if (parseExpression(&expression))
		{
			expect(TokenType::SemiColon, false);
			*outStatement = expression;
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
	bool parseTopLevel(AST::Module* module)
	{
		auto* body = createNode<AST::StatementBody>();
		module->body = body;

		this->currentScope = &body->scope;

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
		assert(!ast->root);		
		auto* node = createNode<AST::Module>();
	
		parseTopLevel(node);
		expect(TokenType::EndOfScan);

		ast->root = node;
		return getParserErrors().size() == 0 && getScannerErrors().size() == 0;
	}

	const vector<ParserError>& getParserErrors() const { return this->parserErrors; }
	const vector<ScannerError>& getScannerErrors() const { return this->scannerFactory.getScannerErrors(); }
	const vector<Token> getTokens() const { return this->tokens; }
	const SourceInput* getSourceInput() const { return this->sourceInput; }

	Parser(const SourceInput* sourceInput, SymbolScope* initialScope = nullptr)
		: scannerFactory(BufferedInputStream(sourceInput->createStream()))
		, baseScanner(scannerFactory.scanTopLevel())
		, sourceInput(sourceInput)
		, currentScope(initialScope)
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

	SymbolScope* currentScope;	
};









