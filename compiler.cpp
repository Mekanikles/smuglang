#include <cassert>
#include <cstdlib>
#include <vector>
#include <string>
#include <iostream>
#include <sstream>
#include <regex>
#include <limits>

#include <fstream>
#include <cctype>

template<typename T>
using vector = std::vector<T>;
using string = std::string;

using uint = unsigned int;

class Indenter
{
public:
	Indenter(int indent) : m_indent(indent) {}
    friend std::ostream& operator<<(std::ostream& out, const Indenter& indenter);  
private:
	int m_indent;
};

std::ostream& operator<<(std::ostream& out, const Indenter& indenter)  
{  
	for (int i = 0; i < indenter.m_indent; ++i)
		out << "    ";	
    return out;  
} 

Indenter indent(int i)
{
	return Indenter(i);
}

void printIndent(int i)
{
	std::clog << indent(i);	
}

void print(const string& str, int i = 0)
{
	printIndent(i);
	std::clog << str;
}

void printLine(const string& str, int i = 0)
{
	printIndent(i);
	std::clog << str << std::endl;
}

void printError(const string& str)
{
	std::cerr << str << std::endl;
}

#define LOG(text) printLine(text)

#define ERROR(text) do { printError(text); exit(-1); } while(false)

enum class TokenType
{
	Import,
	OpenParenthesis,
	CloseParenthesis,
	StringLiteral,
	IntegerLiteral,
	FloatLiteral,
	Symbol,
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

class BufferedInputStream
{
public:
	BufferedInputStream(std::istream& inStream) 
		: m_inStream(inStream)
	{
	}

	operator bool()
	{
		return (m_end != m_begin) || (bool)m_inStream;
	}

	uint currentColumn() { return m_column; }
	uint currentRow() { return m_row; }

	char get()
	{
		char c;
		get(c);
		return c;
	}

	bool get(char& c)
	{
		bool empty = false;
		if (m_end == m_begin)
			empty = fillBuffer() == 0;

		if (!empty)
		{
			c = m_circBuffer[m_begin++];
			m_begin = m_begin % BUF_SIZE;

			if (c == '\n')
			{
				m_row++;
				m_column = 0;
			}
			else
			{
				m_column++;
			}
			//printLine(string("char: '") + c + "', row: " + std::to_string(m_row) + ", column: " + std::to_string(m_column));

			return true;
		}
		else
		{
			c = EOF;
			return false;
		}
	}

	char lookAhead(uint count = 1)
	{
		assert(count < MAX_LOOKAHEAD);
		uint buffered = bufferedLength();
		if (buffered <= count)
			buffered = fillBuffer();

		if (buffered > count)
			return m_circBuffer[(m_begin + count) % BUF_SIZE];
		else
			return EOF;
	}

	void ignore(uint count = 1)
	{
		char c;
		for (int i = 0; i < count; ++i)
			get(c);
	}

	char peek()
	{
		bool empty = false;
		if (m_end == m_begin)
			empty = fillBuffer() == 0;

		if (!empty)
			return m_circBuffer[m_begin];
		else
			return EOF;
	}
private:
	uint bufferedLength()
	{
		return (m_end + BUF_SIZE - m_begin) % BUF_SIZE;
	}

	uint fillBuffer()
	{
		const uint bufSpace = BUF_SIZE - bufferedLength();
		const uint fill = bufSpace - 1; // Leave one char for m_end
		if (fill)
		{
			uint fill1 = std::min(BUF_SIZE - m_end, fill);
			m_inStream.read(&m_circBuffer[m_end], fill1);
			m_end += m_inStream.gcount();

			m_end = m_end % BUF_SIZE;

			uint fill2 = fill - fill1;
			if (fill2)
			{
				m_inStream.read(&m_circBuffer[m_end], fill2);
				m_end += m_inStream.gcount();
			}
		}

		return bufferedLength();
	}

private:
	static const uint MAX_LOOKAHEAD = 63;
	static const uint BUF_SIZE = MAX_LOOKAHEAD + 1;
	char m_circBuffer[BUF_SIZE];
	uint m_begin = 0;
	uint m_end = 0;
	std::istream& m_inStream;
	uint m_row = 1;
	uint m_column = 1;
};

struct ScannerError
{
	string msg;
	uint column;
	uint row;
};

vector<ScannerError> s_scannerErrors;
int s_newScannerErrors = 0;

class Scanner
{
public:	
	Scanner(BufferedInputStream& inStream) 
		: m_inStream(inStream)
	{
	}

	virtual bool getToken(Token* outToken) = 0;

	uint currentColumn() { return m_inStream.currentColumn(); }
	uint currentRow() { return m_inStream.currentRow(); }

protected:
	enum class ScanResult
	{
		FoundToken,
		Error,
		Skip,
		Nothing
	};

	void error(string msg)
	{
		const uint column = currentColumn();
		const uint row = currentRow();

		s_scannerErrors.push_back(ScannerError { msg, column, row });
		s_newScannerErrors++;
	}

	ScanResult scanComments()
	{
		const char n1 = m_inStream.peek();
		const char n2 = m_inStream.lookAhead();

		// Line comment
		if (n1 == '/' && n2 == '/')
		{
			m_inStream.ignore(2);
			char c;
			while (m_inStream.get(c))
			{
				// Skip until eol
				if (c == '\n')
					return ScanResult::Skip;
			}

			// TODO: Error
			return ScanResult::Error;
		}

		// Block comments
		if (n1 == '/' && n2 == '*')
		{
			m_inStream.ignore(2);
			uint level = 1;

			char c;
			while (m_inStream.get(c))
			{
				// Find end blocks
				if (c == '/' && m_inStream.peek() == '*')
				{
					m_inStream.ignore();
					++level;
				}

				// Find end blocks
				if (c == '*' && m_inStream.peek() == '/')
				{
					m_inStream.ignore();
					if (level == 0)
						return ScanResult::Error;

					if (level == 1)
						return ScanResult::Skip; 

					--level;
				}				
			}

			// TODO: Error
			return ScanResult::Error;
		}

		return ScanResult::Nothing;
	}

protected:
	BufferedInputStream& m_inStream;
};

class TopLevelScanner : public Scanner
{
public:	
	using Scanner::Scanner;

	string scanWord()
	{
		string ret;
		if (isalpha(m_inStream.peek()))
		{
			char c;
			m_inStream.get(c);
			ret += c;
			while(isalnum(m_inStream.peek()))
			{
				m_inStream.get(c);
				ret += c;
			}
		}

		return ret;
	}

	bool isBinaryDigit(char c)
	{
		return (c == '0') || (c == '1');
	}

	TokenType scanNumericLiteral(string& out)
	{
		char n = m_inStream.peek();
		if (n == '0')
		{
			// hexadecimal
			if (m_inStream.lookAhead() == 'x')
			{
				out += m_inStream.get();
				out += m_inStream.get();

				assert(isxdigit(m_inStream.peek()));
				out += m_inStream.get();
				while (isxdigit(m_inStream.peek()))
					out += m_inStream.get();

				return TokenType::IntegerLiteral;
			}

			// binary
			if (m_inStream.lookAhead() == 'b')
			{
				out += m_inStream.get();
				out += m_inStream.get();

				assert(isBinaryDigit(m_inStream.peek()));
				out += m_inStream.get();
				while (isBinaryDigit(m_inStream.peek()))
					out += m_inStream.get();

				return TokenType::IntegerLiteral;
			}
		}

		do
		{
			if (isdigit(n))
			{
				out += m_inStream.get();
			}
			else if (n == '.')
			{
				out += m_inStream.get();

				// We know this is a floating type
				while ((n = m_inStream.peek()) != EOF)
				{
					if(isdigit(n))
					{
						out += m_inStream.get();
					}
					else if (n == 'e' || n == 'E')
					{
						out += m_inStream.get();
						assert(isdigit(m_inStream.peek()));
						out += m_inStream.get();
						// We can only digits after exponent sign
						while (isdigit(m_inStream.peek()))
							out += m_inStream.get();
					}
					else
					{
						break;
					}
				}

				return TokenType::FloatLiteral;
			}
			else
			{
				break;
			}
		} while((n = m_inStream.peek()) != EOF);

		return TokenType::IntegerLiteral;
	}

	string scanStringLiteral()
	{
		char c;
		m_inStream.get(c);
		string ret;
		if (c == '"')
		{
			ret += c;
			// TODO: Add error reporing
			while(m_inStream.peek() != '"')
			{
				m_inStream.get(c);
				ret += c;
				assert(m_inStream.peek() != EOF);
				assert(m_inStream.peek() != '\n');
			}
		}
		else if (c == '\'')
		{
			ret += c;
			// TODO: Add error reporing
			while(m_inStream.peek() != '\'')
			{
				m_inStream.get(c);
				ret += c;
				assert(m_inStream.peek() != EOF);
				assert(m_inStream.peek() != '\n');
			}
		}
		else
		{
			assert(false);
		}
		// eat end-of-string identifier
		m_inStream.get(c);
		ret += c;
		return ret;
	}

	bool getToken(Token* outToken) override
	{
		outToken->type = TokenType::EndOfScan;

		while (m_inStream)
		{
			auto scanRes = Scanner::scanComments();
			if (scanRes == ScanResult::FoundToken)
				return true;
			else if (scanRes == ScanResult::Error)
				return false;
			else if (scanRes == ScanResult::Skip)
				continue;

			const char n = m_inStream.peek();
			if (n == ' ' || n == '\t' || n == '\n')
			{
				m_inStream.ignore();
				continue;
			}

			if (n == ';')
			{
				m_inStream.ignore();
				*outToken = Token(TokenType::SemiColon);
				return true;
			}

			if (n == ',')
			{
				m_inStream.ignore();
				*outToken = Token(TokenType::Comma);
				return true;
			}

			// Compiler directive
			if (n == '#')
			{
				m_inStream.ignore();
				string w = scanWord();
				// TODO: Add error for no symbol
				if (w != "")
				{
					*outToken = Token(TokenType::CompilerDirective, w);
					return true;
				}
			}

			// Symbols
			if (isalpha(n))
			{
				string w = scanWord();
				// TODO: Difference between toplevel scan and body scan?
				if (w == "import")
					*outToken = Token(TokenType::Import);
				else	
					*outToken = Token(TokenType::Symbol, w);
				return true;
			}

			// Numericals
			if (isdigit(n) || (n == '.' && isdigit(m_inStream.lookAhead())))
			{
				string literal;
				TokenType type = scanNumericLiteral(literal);

				assert(literal != "");
				if (type == TokenType::FloatLiteral)
					*outToken = Token(TokenType::FloatLiteral, literal);
				else
					*outToken = Token(TokenType::IntegerLiteral, literal);

				return true;
			}

			// Strings
			if (n == '"' || n == '\'')
			{
				string s = scanStringLiteral();
				*outToken = Token(TokenType::StringLiteral, s);
				return true;
			}

			char n2 = m_inStream.lookAhead();

			// double-digit operators
			if (n == '+' && n2 == '+')
			{
				m_inStream.ignore(2);
				*outToken = Token(TokenType::IncrementOp);
				return true;
			}
			else if (n =='-' && n2 == '-')
			{
				m_inStream.ignore(2);
				*outToken = Token(TokenType::DecrementOp);
				return true;
			}

			// single-digit operators
			if (n == '+')
			{
				m_inStream.ignore();
				*outToken = Token(TokenType::AddOp);
				return true;
			}
			else if (n == '-')
			{
				m_inStream.ignore();
				*outToken = Token(TokenType::SubtractOp);
				return true;
			}
			else if (n == '*')
			{
				m_inStream.ignore();
				*outToken = Token(TokenType::MultiplicationOp);
				return true;
			}
			else if (n == '/')
			{
				m_inStream.ignore();
				*outToken = Token(TokenType::DivisionOp);
				return true;
			}

			if (n == '(')
			{
				m_inStream.ignore();
				*outToken = Token(TokenType::OpenParenthesis);
				return true;
			}

			if (n == ')')
			{
				m_inStream.ignore();
				*outToken = Token(TokenType::CloseParenthesis);
				return true;
			}

			// Don't let anything unparsed through
			m_inStream.ignore();			
			error("Unrecognized character");
		}

		return true;
	}
};

class ScannerFactory
{
public:
	ScannerFactory(BufferedInputStream& inStream) 
		: m_inStream(inStream)
	{
	}

	TopLevelScanner scanTopLevel() { return TopLevelScanner(m_inStream); }

private:
	BufferedInputStream& m_inStream;
};

struct AST
{
	struct Statement;
	struct Expression;
	struct Module;
	struct Import;
	struct Call;
	struct StringLiteral;
	struct IntegerLiteral;
	struct FloatLiteral;
	struct UnaryOp;
	struct UnaryPostfixOp;
	struct BinaryOp;

	struct Visitor
	{
		virtual void visit(Statement* node) { assert(false); }
		virtual void visit(Module* node) { assert(false); }
		virtual void visit(Import* node) { visit((Statement*)node); }
		virtual void visit(Call* node) { visit((Statement*)node); }
		virtual void visit(Expression* node) { assert(false); }
		virtual void visit(StringLiteral* node) { visit((Expression*)node);}
		virtual void visit(IntegerLiteral* node) { visit((Expression*)node); }
		virtual void visit(FloatLiteral* node) { visit((Expression*)node);  }
		virtual void visit(UnaryOp* node) { visit((Expression*)node);  }
		virtual void visit(UnaryPostfixOp* node) { visit((Expression*)node); }
		virtual void visit(BinaryOp* node) { visit((Expression*)node);  }
	};

	struct Node
	{
		virtual ~Node() = default;

		virtual const vector<Node*> getChildren() { return vector<Node*>(); }
		virtual void accept(Visitor* v) = 0;
		virtual string toString() = 0;
	};

	struct Statement : Node
	{};

	struct Expression : Node
	{};

	template<typename T, typename P = Node>
	struct NodeImpl : P
	{
		void accept(Visitor* v) override { v->visit((T*)this); }
	};

	struct Module : NodeImpl<Module>
	{
		vector<Statement*> statements;
		string toString() override { return "Module"; }
		const vector<Node*> getChildren() override
		{
			return vector<Node*>(statements.begin(), statements.end());
		}

		void addStatement(Statement* s)
		{
			statements.push_back(s);
		}
	};

	struct Import : NodeImpl<Import, Statement>
	{	
		enum Type
		{
			Type_Native,
			Type_C,
		}; 
		Type type;

		string file;
		string toString() override { return string("Import(file:") + file + ")"; }	
	};

	struct StringLiteral : NodeImpl<StringLiteral, Expression>
	{
		string value;
		string toString() override 
		{ 
			string s = "StringLiteral(" + value + ")";
			return s;
		}
	};

	struct IntegerLiteral : NodeImpl<IntegerLiteral, Expression>
	{
		string value;
		string toString() override 
		{ 
			string s = "IntegerLiteral(" + value + ")";
			return s;
		}
	};

	struct FloatLiteral : NodeImpl<FloatLiteral, Expression>
	{
		string value;
		string toString() override 
		{ 
			string s = "FloatLiteral(" + value + ")";
			return s;
		}
	};

	// TODO: split functions into calls and call-expression?
	//	for funcs that can act as an expression/operand
	struct Call : NodeImpl<Call, Statement>
	{
		string function;
		vector<Expression*> args;
		string toString() override 
		{ 
			string s = "Call(" + function + ")";
			return s; 
		}
		const vector<Node*> getChildren() override
		{
			return vector<Node*>(args.begin(), args.end());
		}
	};

	struct UnaryOp : NodeImpl<UnaryOp, Expression>
	{
		TokenType type;
		Expression* expr;

		string toString() override 
		{ 
			string s = "UnaryOp(" + ::toString(type) + ")";
			return s; 
		}
		const vector<Node*> getChildren() override
		{
			vector<Node*> ret;
			ret.reserve(1);
			ret.push_back(expr);
			return ret;
		}
	};

	struct UnaryPostfixOp : NodeImpl<UnaryPostfixOp, Expression>
	{
		TokenType type;
		Expression* expr;

		string toString() override 
		{ 
			string s = "UnaryPostfixOp(" + ::toString(type) + ")";
			return s; 
		}
		const vector<Node*> getChildren() override
		{
			vector<Node*> ret;
			ret.reserve(1);
			ret.push_back(expr);
			return ret;
		}
	};

	struct BinaryOp : NodeImpl<BinaryOp, Expression>
	{
		TokenType type;
		Expression* left;
		Expression* right;

		string toString() override 
		{ 
			string s = "BinaryOp(" + ::toString(type) + ")";
			return s; 
		}
		const vector<Node*> getChildren() override
		{
			vector<Node*> ret;
			ret.reserve(2);
			ret.push_back(left);
			ret.push_back(right);
			return ret;
		}
	};

	Node* root = nullptr;
};

vector<AST::Node*> s_nodes;

template<typename NodeT, typename... Args>
NodeT* createNode(Args... args)
{
	NodeT* n = new NodeT(std::forward<Args>(args)...);
	s_nodes.push_back(n);
	return n;
}

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

void printAST(AST* ast, int indent = 0)
{
	/*
		Module
		|-Function1
		| |-Expr
		|   |-Expr1
		| 	| |-Op1
		| 	| |-Op2
		|	|-Epxr2
		|	  |-Op1
		|	  |-Op2
		|-Function2
	*/

	vector<bool> depthStack;
	std::function<void(AST::Node*)> rec = [&](AST::Node* node)
	{
		printIndent(indent);
		const int depth = depthStack.size();
		if (depth > 0)
		{
			for (int i = 0; i < depth - 1; ++i)
			{
				if (depthStack[i])
					print("| ");
				else
					print("  ");
			}
			print("|-");
		}

		printLine(node->toString());

		const auto children = node->getChildren();
		const int childCount = children.size();
		if (childCount > 0)
		{
			depthStack.push_back(true);
			for (int i = 0; i < childCount - 1; ++i)
			{
				rec(children[i]);
			}
			depthStack.back() = false;
			rec(children.back());
			depthStack.pop_back();
		}
	};

	rec(ast->root);
}

void printTokens(const vector<Token>& tokens)
{
	for (auto& t : tokens)
	{
		printLine(toString(t), 1);
	}
}

struct CGenerator : AST::Visitor
{
	CGenerator(std::ostream* out)
		: m_out(out)
		, m_indent(0)
	{}

	void run(AST* ast)
	{
		ast->root->accept(this);

		auto& out = *m_out;
	
		out << m_head.str();
		out << m_body.str();
	}

	void visit(AST::Module* node) override
	{
		auto& out = m_body;
		out << "int main()\n{\n";
		{
			m_indent++;
			for (auto s : node->statements)
			{
				s->accept(this);
			}
		}
		out << "}\n";
	}

	void visit(AST::Import* node) override
	{
		auto& out = m_head;

		if (node->type == AST::Import::Type_C)
			out << "#include <" << node->file << ">\n";
	}

	void visit(AST::Call* node) override
	{
		auto& out = m_body;

		out << indent(m_indent) << node->function << "(";
		int argCount = node->args.size();
		if (argCount > 0)
		{
			node->args[0]->accept(this);
			for (int i = 1; i < argCount; ++i)
			{
				out << ", ";
				node->args[i]->accept(this);
			}
		}
		out << ");\n";
	};

	void visit(AST::BinaryOp* node) override
	{	
		auto& out = m_body;

		out << "(";
		node->left->accept(this);
		out << " ";
		switch (node->type)
		{
			case TokenType::AddOp: out << "+ "; break;
			case TokenType::SubtractOp: out << "- "; break;
			case TokenType::MultiplicationOp: out << "* "; break;
			case TokenType::DivisionOp: out << "/ "; break;
			default: assert(false);
		}
		node->right->accept(this);
		out << ")";
	}

	void visit(AST::UnaryOp* node) override
	{
		auto& out = m_body;

		switch (node->type)
		{
			case TokenType::AddOp: out << "+ "; break;
			case TokenType::SubtractOp: out << "- "; break;
			case TokenType::IncrementOp: out << "++ "; break;
			case TokenType::DecrementOp: out << "-- "; break;
			default: assert(false);
		}
		node->expr->accept(this);
	}

	void visit(AST::UnaryPostfixOp* node) override
	{
		auto& out = m_body;

		node->expr->accept(this);

		switch (node->type)
		{
			case TokenType::IncrementOp: out << "++ "; break;
			case TokenType::DecrementOp: out << "-- "; break;
			default: assert(false);
		}
	}

	void visit(AST::StringLiteral* node) override
	{	m_body << node->value; }

	void visit(AST::IntegerLiteral* node) override
	{	m_body << node->value; }

	void visit(AST::FloatLiteral* node) override
	{	m_body << node->value; }

	std::ostream* m_out;
	std::stringstream m_head;
	std::stringstream m_body;
	int m_indent;
	unsigned m_tempVarCount = 0;
}; 


std::ifstream& gotoLine(std::ifstream& file, unsigned int num){
    file.seekg(std::ios::beg);
    for(int i=0; i < num - 1; ++i){
        file.ignore(std::numeric_limits<std::streamsize>::max(),'\n');
    }
    return file;
}

void printPointAtColumn(int column, int indent = 0)
{
	indent = ((column - 1) / 4) + 1;
	printIndent(indent);
	int rest = std::max((column - 1) % 4, 0);
	for (int i = 0; i < rest; ++i)
		print(" ");
	printLine("\033[1;31m^\033[0m");
}

void printScannerErrors(std::ifstream& file)
{
	for (auto e : s_scannerErrors)
	{
		printLine(string("\033[1m") + std::to_string(e.row) + 
				":" + std::to_string(e.column) + ": \033[31mError: \033[39m" + e.msg + "\033[0m");

		// TODO: Make into segment surrounding error instead, to support really long lines
		char line[256];
		auto& fileAtLine = gotoLine(file, e.row);
		fileAtLine.getline(line, 256);
		printLine(string(line), 1);
		printPointAtColumn(e.column, 1);
	}
}

void printParserErrors(std::ifstream& file)
{
	for (auto e : s_parserErrors)
	{
		printLine(string("\033[1m") + std::to_string(e.row) + 
				":" + std::to_string(e.column) + ": \033[31mError: \033[39m" + e.msg + "\033[0m");

		// TODO: Make into segment surrounding error instead, to support really long lines
		char line[256];
		auto& fileAtLine = gotoLine(file, e.row);
		fileAtLine.getline(line, 256);
		printLine(string(line), 1);
		printPointAtColumn(e.column, 1);
	}
}

int main(int argc, char** argv)
{
	vector<string> args(argv + 1, argv + argc);

	if (args.size() < 1)
		ERROR("No input files specified");

	std::ifstream inFile(args[0]);

	BufferedInputStream inStream(inFile);
	ScannerFactory scannerFactory(inStream);
	AST ast;

	LOG("Parsing...");
	if (!parse(&scannerFactory, &ast))
	{
		LOG("Parse fail!");
		auto f = std::ifstream(args[0]);
		printScannerErrors(f);
		printParserErrors(f);
	}
	else
	{
		LOG("Parse success!");
		printLine("Tokens:");
		printTokens(s_tokens);

		printLine("AST:");
		printAST(&ast, 1);

		std::stringstream output;
		CGenerator generator(&output);
		generator.run(&ast);

		std::regex filenameRegex(R"((.*[\\\/])?(.+)$))");
		std::smatch matches;

		assert(std::regex_search(args[0], matches, filenameRegex));
		{
			string outFileName = string(".smug/") + matches[2].str() + ".smc";
			std::ofstream outFile(outFileName);

			printLine("Generated C:");
			string l;
			while (getline(output, l))
			{
				printLine(l, 1);
				outFile << l << std::endl;
			}
			std::cout << outFileName;
		}
	}
}

