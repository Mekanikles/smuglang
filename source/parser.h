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

	void error(const Token& token, string msg)
	{
		auto* s = getScanner();
		const uint column = s->currentColumn();
		const uint row = s->currentRow();
		const int tokenLength = std::max((int)token.symbol.length() - 1, 0);

		this->parserErrors.push_back(ParserError { msg, token, column - tokenLength, row });
		this->newErrors++;
	}

	void error(string msg)
	{
		error(lastToken(), msg);
	}

	int getNewErrors() { const int ret = this->newErrors; this->newErrors = 0; return ret; }

	void advanceToken()
	{
		this->tokens.push_back(this->currentToken);
		getScanner()->getToken(&this->currentToken);
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
		error(this->currentToken, errMsg);
		if (skipOnError)
			advanceToken();	
		return false;
	}

	bool acceptUnaryOperator()
	{
		if (accept(TokenType::AddOp))
		{
			 return true;
		}
		else if (accept(TokenType::SubtractOp))
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
		else if (accept(TokenType::AddOp))
		{
			 return true;
		}
		else if (accept(TokenType::SubtractOp))
		{
			 return true;
		}
		else if (accept(TokenType::MultiplicationOp))
		{
			 return true;
		}
		else if (accept(TokenType::DivisionOp))
		{
			 return true;
		}		
		return false;
	}

	bool parsePrimitiveTypeExpression(AST::Expression** outNode)
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

				if (lastToken().symbol == "s64")
				{
					sign = PrimitiveClass::Signed;
					size = 64;
				}
				else if (lastToken().symbol == "u64")
				{
					sign = PrimitiveClass::Unsigned;
					size = 64;
				}
				if (lastToken().symbol == "s32")
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
					error("Unknown primitive type");
				}

				auto typeClass = std::make_unique<PrimitiveClass>(PrimitiveClass::Int, size, sign);
				auto* node = createNode<AST::TypeLiteral>(std::move(typeClass));		
				*outNode = node;
			}
			else
			{
				error("Cannot parse compiler directive as expression");
			}

			return true;
		}

		return false;
	}

	bool parsePrimaryExpression(AST::Expression** outNode)
	{
		AST::SymbolExpression* symExpr;
		if (accept(TokenType::OpenParenthesis))
		{
			if (!parseExpression(outNode))
				error("Expected expression");

			expect(TokenType::CloseParenthesis);
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
				error("Expected primary expression");
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
			auto* expr = createNode<AST::SymbolExpression>(lastToken().symbol);
			*outNode = expr;
		}
		else if (parsePrimitiveTypeExpression(outNode))
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
			case TokenType::MultiplicationOp:
			case TokenType::DivisionOp:
				return 10;
			case TokenType::AddOp:
			case TokenType::SubtractOp:
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

			if (parsePrimaryExpression(&exprNode))
			{
				exprStack.push_back(exprNode);
				continue;
			}
			else
			{
				error("Expected primary expression");
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

	// TODO: Expand this to any expression
	bool parseTypeExpression(AST::Expression** outExpr)
	{
		if (accept(TokenType::Symbol))
		{
			auto* node = createNode<AST::SymbolExpression>(lastToken().symbol);
			*outExpr = node;
			return true;
		}
		else if (parsePrimitiveTypeExpression(outExpr))
		{
			return true;
		}

		return false;
	}

	bool parseSymbolDeclaration(AST::SymbolDeclaration** outDeclaration)
	{
		if (!accept(TokenType::Symbol))
			return false;
		
		auto* node = createNode<AST::SymbolDeclaration>();
		*outDeclaration = node;

		node->symbol = lastToken().symbol;

		// Optional type declaration
		if (accept(TokenType::Colon))
		{
			// TODO: Should handle generic expressions
			AST::Expression* expr;
			if (!parseTypeExpression(&expr))
			{
				error("Expected expression");
			}

			node->typeExpr = expr;
		}

		// Optional initialization
		if (accept(TokenType::Equals))
		{
			AST::Expression* expr;
			if (parseExpression(&expr))
			{
				node->initExpr = expr;
			}
			else
			{
				error("Expected expression");
			}
		}

		return true;
	}

	bool parseParamDeclaration(AST::SymbolDeclaration** outDeclaration)
	{
		AST::SymbolDeclaration* symbolDecl;
		if (parseSymbolDeclaration(&symbolDecl))
		{
			symbolDecl->isParam = true;
			*outDeclaration = symbolDecl;
			return true;
		}
		return false;
	}

	bool parseFuncLiteralSignature(AST::FuncLiteralSignature** outSignature)
	{
		if (accept(TokenType::OpenParenthesis))
		{
			auto* node = createNode<AST::FuncLiteralSignature>();
			*outSignature = node;

			AST::SymbolDeclaration* declNode;
			while (parseParamDeclaration(&declNode))
			{
				node->addParameterDeclaration(declNode);

				if (accept(TokenType::Comma))
				{
					continue;
				}
				else
				{
					break;
				}
			}

			// TODO:: Add output params

			expect(TokenType::CloseParenthesis);

			return true;
		}

		return false;
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

	bool parseFunctionDeclaration(bool external, AST::FunctionDeclaration** outDeclaration)
	{
		if (accept(TokenType::Func))
		{
			auto* node = createNode<AST::FunctionDeclaration>();
			*outDeclaration = node;

			if (!expect(TokenType::Symbol))
				return true;

			node->symbol = lastToken().symbol;

			AST::FuncLiteralSignature* signature;
			if (!external && !parseFuncLiteralSignature(&signature))
			{
				// TODO: Require parameter list?
				error("Expected function parameter list");
				*outDeclaration = nullptr;
				return true;
			}

			AST::StatementBody* statementBody = nullptr;
			if (!external && !parseStatementBody(&statementBody))
			{
				error("Expected statement body");
			}

			if (!external)
			{
				auto* func = createNode<AST::FunctionLiteral>();
				func->signature = signature;
				func->body = statementBody;

				node->funcLiteral = func;
			}

			*outDeclaration = node;
			return true;
		}

		return false;
	}

	// TODO: Should output statement?
	bool parseDeclarationStatement(AST::Declaration** outDeclaration)
	{
		bool isExternalDecl = false;
		// Optional external declaration qualifier
		if (accept(TokenType::Extern))
		{
			isExternalDecl = true;
		}

		AST::FunctionDeclaration* funcDecl;
		if (accept(TokenType::Var))
		{
			// TODO: use-case?
			if (isExternalDecl)
				error("External non-function declarations not yet supported");

			AST::SymbolDeclaration* symbolDecl;
			if (!parseSymbolDeclaration(&symbolDecl))
			{
				*outDeclaration = nullptr;
				return true;
			}

			// TODO: Set storage qualifier
			*outDeclaration = symbolDecl;
			return true;
		}
		else if (parseFunctionDeclaration(isExternalDecl, &funcDecl))
		{

			*outDeclaration = funcDecl;
			return true;
		}
		else if (isExternalDecl)
		{
			error("Expected declaration statement");
			return true;
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
				error("Expected expression");
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
				error("Expected statement");
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
					error("Expected statement");
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
				error("Expected expression");
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

		if (accept(TokenType::Import))
		{
			AST::Import::Type importType = AST::Import::Type_Native;

			// optional import params
			// TODO: invoke new parameter list scanner?
			if (accept(TokenType::OpenParenthesis))
			{
				expect(TokenType::StringLiteral);	
				if (stringSymbolValue(lastToken().symbol) != "c")
					error(lastToken(), "Only 'c' imports type specifier is supported");

				importType = AST::Import::Type_C;

				expect(TokenType::CloseParenthesis);	
			}
		
			expect(TokenType::StringLiteral);
			const string file = stringSymbolValue(lastToken().symbol);

			expect(TokenType::SemiColon);

			auto node = createNode<AST::Import>();
			node->file = file;
			if(importType != AST::Import::Type_C)
				error(lastToken(), "Only 'c' type imports are supported");
			node->type = importType;
			*outStatement = node;
		}
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
					error(lastToken(), "Call parameter list expected");
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
					error("Expected expression");
				}

				node->symExpr = symExpr;
				node->expr = expr;

				expect(TokenType::SemiColon, false);
			}
			else
			{
				error("Cannot parse lone symbol");
				return false;
			}
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
				error("Expected statement");
				skipToNextStatement();
			}
		}

		return false;
	}

	bool parse(AST::AST* ast)
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
		: scannerFactory(std::move(BufferedInputStream(sourceInput->createStream())))
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

	SymbolScope* currentScope;	
};









