#pragma once

enum class TokenType
{
	Import,
	Extern,
	OpenParenthesis,
	CloseParenthesis,
	OpenBrace,
	CloseBrace,
	StringLiteral,
	IntegerLiteral,
	FloatLiteral,
	Symbol,
	Var,
	Func,
	If,
	Else,
	Equals,
	CompilerDirective,
	SemiColon,
	Comma,
	Colon,
	Dot,
	CompareOp,
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
		case TokenType::Extern: return "Extern";
		case TokenType::OpenParenthesis: return "Opening Parenthesis";
		case TokenType::CloseParenthesis: return "Closing Parenthesis";
		case TokenType::OpenBrace: return "Opening brace";
		case TokenType::CloseBrace: return "Closing brace";
		case TokenType::StringLiteral: return "String Literal";
		case TokenType::IntegerLiteral: return "Integer Literal";
		case TokenType::FloatLiteral: return "Float Literal";
		case TokenType::Symbol: return "Symbol";
		case TokenType::Var: return "Var";
		case TokenType::Func: return "Func";
		case TokenType::If: return "If";
		case TokenType::Else: return "Else";
		case TokenType::Equals: return "Equals";
		// TODO: # should be its own token, compiler directive should be on parser level
		case TokenType::CompilerDirective: return "CompilerDirective";		
		case TokenType::SemiColon: return "Semi Colon";
		case TokenType::Comma: return "Comma";
		case TokenType::Colon: return "Colon";
		case TokenType::Dot: return "Dot";
		case TokenType::CompareOp: return "Operator compare";
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
