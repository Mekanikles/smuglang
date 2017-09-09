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

bool expect(TokenType type)
{
	//printLine(string("Expect: ") + toString(type) + ((s_currentToken.type != type)? (string(" (got ") + toString(s_currentToken.type) + ")") : ""));
	if (accept(type))
		return true;
	
	string errMsg = 
			string("Expected ") + toString(type) + " but got " + 
			toString(s_currentToken.type);
	error(s_currentToken, errMsg);
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
	if (accept(TokenType::AddOp))
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

bool parsePrimaryExpression(AST::Expression** outNode)
{
	if (accept(TokenType::OpenParenthesis))
	{
		if (!parseExpression(outNode))
			error("Expected expression");

		expect(TokenType::CloseParenthesis);
	}
	else if (acceptUnaryOperator())
	{
		auto uop = createNode<AST::UnaryOp>();
		uop->type = lastToken().type;

		AST::Expression* expr = nullptr;
		if (parsePrimaryExpression(&expr))
		{
			uop->expr = expr;
			*outNode = uop;
		}
		else
		{
			error("Expected primary expression");
			return true;
		}
	}
	else if (accept(TokenType::StringLiteral))
	{
		auto* expr = createNode<AST::StringLiteral>();
		expr->value = lastToken().symbol;
		*outNode = expr;
	}
	else if (accept(TokenType::IntegerLiteral))
	{
		auto* expr = createNode<AST::IntegerLiteral>();
		expr->value = lastToken().symbol;
		*outNode = expr;
	}
	else if (accept(TokenType::FloatLiteral))
	{
		auto* expr = createNode<AST::FloatLiteral>();
		expr->value = lastToken().symbol;
		*outNode = expr;
	}
	else
	{
		return false;
	}

	if (acceptUnaryPostfixOperator())
	{
		auto uop = createNode<AST::UnaryPostfixOp>();
		uop->type = lastToken().type;
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
			return 2;
		case TokenType::AddOp:
		case TokenType::SubtractOp:
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
			bop->type = binOpStack.back();
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
			return true;
		}
	}

	// Take care of remaining ops
	while (binOpStack.size() > 0)
	{
		auto* bop = createNode<AST::BinaryOp>();
		bop->type = binOpStack.back();
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
	if (accept(TokenType::OpenParenthesis))
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

		expect(TokenType::CloseParenthesis);

		return true;
	}
	return false;
}

string stringSymbolValue(string symbol)
{
	return symbol.substr(1, symbol.length() - 2);
}


void skipCurrentStatement()
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

// TODO: Rename parseModule
bool parseTopLevel(AST::Module* module)
{
	while(true)
	{
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
			module->addStatement(node);
		}
		else if (accept(TokenType::Symbol))
		{
			Token t = lastToken();
			string symbol = t.symbol;

			// Function call
			if (peek(TokenType::OpenParenthesis))
			{
				auto node = createNode<AST::Call>();
				node->function = symbol;

				if (!parseCallParameters(node))
				{
					error(lastToken(), "Call parameter list expected");
				}

				if (!newErrors())
				{
					expect(TokenType::SemiColon);

					module->addStatement(node);
				}
				else
				{
					skipCurrentStatement();
				}		
			}
			else
			{
				error(t, "No support for non-call symbols yet!");
			}
		}
		else if (newErrors())
		{
			skipCurrentStatement();
		}
		else
		{
			// TODO: Can module parse return false?
			return true;
		}
	}

	return false;
}


bool parse(ScannerFactory* scannerFactory, AST* ast)
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
