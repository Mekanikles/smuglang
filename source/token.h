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
	HexadecimalLiteral,
	BinaryLiteral,
	FloatLiteral,
	Symbol,
	At,
	Var,
	Const,
	Def,
	Func,
	Struct,
	Eval,
	Defer,
	Return,
	If,
	Else,
	Equals,
	True,
	False,
	Loop,
	Break,
	Continue,
	CompilerDirective,
	TemplateParameterOpener,
	SemiColon,
	Comma,
	Colon,
	Dot,
	Ampersand,
	EqualsOp,
	LessThanOp,
	GreaterThanOp,
	LessThanOrEqualsOp,
	GreaterThanOrEqualsOp,
	Plus,
	Minus,
	Asterisk,
	Slash,
	Modulo,
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

	operator bool() const
	{
		return type != TokenType::Invalid;
	}
};

const Token s_invalidToken(TokenType::Invalid);

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
		case TokenType::HexadecimalLiteral: return "Hexadecimal Literal";
		case TokenType::BinaryLiteral: return "Binary Literal";
		case TokenType::FloatLiteral: return "Float Literal";
		case TokenType::Symbol: return "Symbol";
		case TokenType::At: return "At";
		case TokenType::Var: return "Var";
		case TokenType::Const: return "Const";
		case TokenType::Def: return "Def";
		case TokenType::Func: return "Func";
		case TokenType::Struct: return "Struct";
		case TokenType::Eval: return "Eval";
		case TokenType::Defer: return "Defer";
		case TokenType::Return: return "Return";
		case TokenType::If: return "If";
		case TokenType::Else: return "Else";
		case TokenType::Equals: return "Equals";
		case TokenType::True: return "True";
		case TokenType::False: return "False";
		case TokenType::Loop: return "Loop";
		case TokenType::Break: return "Break";
		case TokenType::Continue: return "Continue";
		// TODO: # should be its own token, compiler directive should be on parser level
		case TokenType::CompilerDirective: return "CompilerDirective";
		case TokenType::TemplateParameterOpener: return "TemplateParameterOpener";	
		case TokenType::SemiColon: return "Semi Colon";
		case TokenType::Comma: return "Comma";
		case TokenType::Colon: return "Colon";
		case TokenType::Dot: return "Dot";
		case TokenType::Ampersand: return "Ampersand";
		case TokenType::EqualsOp: return "Equals Op";
		case TokenType::LessThanOp: return "LessThan Op";
		case TokenType::GreaterThanOp: return "GreaterThan Op";
		case TokenType::LessThanOrEqualsOp: return "LessThanOrEquals Op";
		case TokenType::GreaterThanOrEqualsOp: return "GreaterThanOrEquals Op";
		case TokenType::Plus: return "Plus";
		case TokenType::Minus: return "Minus";
		case TokenType::Asterisk: return "Asterisk";
		case TokenType::Slash: return "Slash";
		case TokenType::Modulo: return "Modulo";
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
