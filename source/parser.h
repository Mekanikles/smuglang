#pragma once

struct ParserError
{
	string msg;
	Token token;
	uint column;
	uint row;
};

vector<Token> s_tokens;
Token s_currentToken = Token(TokenType::StartOfScan);
vector<Scanner*> s_scanners;
vector<ParserError> s_parserErrors;
int s_newErrors = 0;

void pushScanner(Scanner* scanner)
{
	s_scanners.push_back(scanner);
}

void popScanner()
{
	s_scanners.pop_back();
}

template<typename T>
struct ScopedScanner
{
	ScopedScanner(T s) : scanner(s) { pushScanner(&scanner); }
	~ScopedScanner() { popScanner(); }

	T scanner;
};

Scanner* getScanner()
{
	return s_scanners.back();
}

const Token& lastToken()
{
	return s_tokens.back();
}

void error(const Token& token, string msg)
{
	auto* s = getScanner();
	const uint column = s->currentColumn();
	const uint row = s->currentRow();
	const int tokenLength = std::max((int)token.symbol.length() - 1, 0);

	s_parserErrors.push_back(ParserError { msg, token, column - tokenLength, row });
	s_newErrors++;
}

void error(string msg)
{
	error(lastToken(), msg);
}

int newErrors() { const int ret = s_newErrors; s_newErrors = 0; return ret; }

void advanceToken()
{
	s_tokens.push_back(s_currentToken);
	getScanner()->getToken(&s_currentToken);
	//printLine(string("Found token: ") + toString(s_currentToken));
}

bool peek(TokenType type)
{
	return s_currentToken.type == type;
}

bool accept(TokenType type)
{
	//printLine(string("Accept: ") + toString(type) + ((s_currentToken.type != type)?  (string(" (got ") + toString(s_currentToken.type) + ")") : ""));	
	if (peek(type))
	{
		advanceToken();
		return true;
	}
	return false;
}

bool expect(TokenType type, bool skipOnError = true)
{
	//printLine(string("Expect: ") + toString(type) + ((s_currentToken.type != type)? (string(" (got ") + toString(s_currentToken.type) + ")") : ""));
	if (accept(type))
		return true;
	
	string errMsg = 
			string("Expected ") + toString(type) + " but got " + 
			toString(s_currentToken.type);
	error(s_currentToken, errMsg);
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

bool parseExpression(AST::Expression** outExpr);

bool parsePrimitiveTypeExpression(AST::Expression** outNode)
{
	if (accept(TokenType::CompilerDirective))
	{
		if (lastToken().symbol == "primitives")
		{
			// TODO: Generalise primitives as list of types
			expect(TokenType::Dot);

			expect(TokenType::Symbol);
			assert(lastToken().symbol == "s32");

			auto typeClass = std::make_unique<PrimitiveClass>(PrimitiveClass::Int, 32);
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

bool parseStatement(AST::Statement** outStatement);

SymbolScope* s_currentScope = nullptr;
bool parseStatementBody(AST::StatementBody** outStatementBody)
{
	if (accept(TokenType::OpenBrace))
	{
		auto* node = createNode<AST::StatementBody>();
		node->scope.parentScope = s_currentScope;
		s_currentScope = &node->scope;

		*outStatementBody = node;

		AST::Statement* statement;
		while(parseStatement(&statement))
		{
			node->addStatement(statement);
		}

		expect(TokenType::CloseBrace);

		s_currentScope = node->scope.parentScope;

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

bool parseStatement(AST::Statement** outStatement)
{
	AST::Declaration* declaration;
	AST::IfStatement* ifStatement;
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

			if (!newErrors())
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

	s_currentScope = &body->scope;

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


bool parse(ScannerFactory* scannerFactory, AST::AST* ast)
{
	// TODO: Should scanner be pushed/popped? All parsers need to do it to be safe
	auto s = ScopedScanner<TopLevelScanner>(scannerFactory->scanTopLevel());

	assert(!ast->root);	
	auto* node = createNode<AST::Module>();

	advanceToken();
	parseTopLevel(node);
	expect(TokenType::EndOfScan);

	ast->root = node;
	return s_parserErrors.size() == 0 && s_scannerErrors.size() == 0;
}
