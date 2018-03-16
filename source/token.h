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
	At,
	Var,
	Func,
	Eval,
	If,
	Else,
	Equals,
	CompilerDirective,
	SemiColon,
	Comma,
	Colon,
	Dot,
	Ampersand,
	CompareOp,
	Plus,
	Minus,
	Asterisk,
	Slash,
	IncrementOp,
	DecrementOp,
	Ellipsis,
	Arrow,
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
		case TokenType::At: return "At";
		case TokenType::Var: return "Var";
		case TokenType::Func: return "Func";
		case TokenType::Eval: return "Eval";
		case TokenType::If: return "If";
		case TokenType::Else: return "Else";
		case TokenType::Equals: return "Equals";
		// TODO: # should be its own token, compiler directive should be on parser level
		case TokenType::CompilerDirective: return "CompilerDirective";		
		case TokenType::SemiColon: return "Semi Colon";
		case TokenType::Comma: return "Comma";
		case TokenType::Colon: return "Colon";
		case TokenType::Dot: return "Dot";
		case TokenType::Ampersand: return "Ampersand";
		case TokenType::CompareOp: return "Compare op";
		case TokenType::Plus: return "Plus";
		case TokenType::Minus: return "Minus";
		case TokenType::Asterisk: return "Asterisk";
		case TokenType::Slash: return "Slash";
		case TokenType::IncrementOp: return "Operator increment";
		case TokenType::DecrementOp: return "Operator decrement";
		case TokenType::Ellipsis: return "Ellipsis";
		case TokenType::Arrow: return "Arrow";
		case TokenType::StartOfScan: return "Start Of Scan";
		case TokenType::EndOfScan: return "End Of Scan";	
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
