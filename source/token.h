#pragma once

enum class TokenType
{
	Import,
	OpenParenthesis,
	CloseParenthesis,
	StringLiteral,
	IntegerLiteral,
	FloatLiteral,
	Symbol,
	Var,
	CompilerDirective,
	SemiColon,
	Comma,
	AddOp,
	SubtractOp,
	MultiplicationOp,
	DivisionOp,
	IncrementOp,
	DecrementOp,
	StartOfScan,
	EndOfScan,
	Invalid,
};

struct Token
{
	TokenType type = TokenType::Invalid;
	string symbol;

	Token() {}

	Token(TokenType type, const string& symbol = "")
		: type(type), symbol(symbol)
	{}
};

string toString(TokenType type)
{
	switch (type)
	{
		case TokenType::Import: return "Import";
		case TokenType::OpenParenthesis: return "Opening Parenthesis";
		case TokenType::CloseParenthesis: return "Closing Parenthesis";
		case TokenType::StringLiteral: return "String Literal";
		case TokenType::IntegerLiteral: return "Integer Literal";
		case TokenType::FloatLiteral: return "Float Literal";
		case TokenType::Symbol: return "Symbol";
		case TokenType::Var: return "Var";
		case TokenType::CompilerDirective: return "Compiler Directive";		
		case TokenType::SemiColon: return "Semi Colon";
		case TokenType::Comma: return "Comma";
		case TokenType::AddOp: return "Operator add";
		case TokenType::SubtractOp: return "Operator subtract";
		case TokenType::MultiplicationOp: return "Operator multiplication";
		case TokenType::DivisionOp: return "Operator division";
		case TokenType::IncrementOp: return "Operator increment";
		case TokenType::DecrementOp: return "Operator decrement";
		case TokenType::StartOfScan: return "Start Of File";
		case TokenType::EndOfScan: return "End Of File";	
		default: return "UknownToken";
	};
}

string toString(const Token& t)
{
	if (t.symbol.length() > 0)
		return toString(t.type) + "(" + t.symbol + ")";
	else
		return toString(t.type);
}
