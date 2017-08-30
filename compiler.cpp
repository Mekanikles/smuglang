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
	PlusSign,
	MinusSign,
	MultiplicationSign,
	DivisionSign,
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
		case TokenType::PlusSign: return "Plus Sign";
		case TokenType::MinusSign: return "Minus Sign";
		case TokenType::MultiplicationSign: return "Multiplication Sign";
		case TokenType::DivisionSign: return "Division Sign";
		case TokenType::StartOfScan: return "Start Of File";
		case TokenType::EndOfScan: return "End Of File";	
		default: return "UknownToken";
	};
}

string toString(const Token& t)
{
	printLine(string() + "Tokensymbol: '" + t.symbol + "' length: " + std::to_string(t.symbol.length()));
	if (t.symbol.length() > 0)
		return toString(t.type) + "(" + t.symbol + ")";
	else
		return toString(t.type);
}

struct ParseError
{
	string error;
};

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
			return m_circBuffer[m_begin + count];
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

class Scanner
{
public:	
	Scanner(BufferedInputStream& inStream) 
		: m_inStream(inStream)
	{
	}

	virtual bool getToken(Token* outToken, ParseError* outError) = 0;

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

	ScanResult scanComments(ParseError* outParseError)
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
	int m_blockCommentLevel = 0;
};

class TopLevelScanner : public Scanner
{
public:	
	using Scanner::Scanner;

	string parseWord()
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

	TokenType parseNumericLiteral(string& out)
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

	string parseStringLiteral()
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

	bool getToken(Token* outToken, ParseError* outError) override
	{
		ParseError error;
		outToken->type = TokenType::EndOfScan;

		while (m_inStream)
		{
			auto scanRes = Scanner::scanComments(outError);
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
				string w = parseWord();
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
				string w = parseWord();
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
				TokenType type = parseNumericLiteral(literal);

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
				string s = parseStringLiteral();
				*outToken = Token(TokenType::StringLiteral, s);
				return true;
			}

			// operators
			if (n == '+')
			{
				m_inStream.ignore();
				*outToken = Token(TokenType::PlusSign);
				return true;
			}
			else if (n == '-')
			{
				m_inStream.ignore();
				*outToken = Token(TokenType::MinusSign);
				return true;
			}
			else if (n == '*')
			{
				m_inStream.ignore();
				*outToken = Token(TokenType::MultiplicationSign);
				return true;
			}
			else if (n == '/')
			{
				m_inStream.ignore();
				*outToken = Token(TokenType::DivisionSign);
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
			assert(false);
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
	struct Module;
	struct Import;
	struct Call;
	struct StringLiteral;
	struct IntegerLiteral;
	struct FloatLiteral;

	struct Visitor
	{
		virtual void visit(Statement* node) { assert(false); }
		virtual void visit(Module* node) { assert(false); }
		virtual void visit(Import* node) { visit((Statement*)node); }
		virtual void visit(Call* node) { visit((Statement*)node); }
		virtual void visit(StringLiteral* node) { assert(false); }
		virtual void visit(IntegerLiteral* node) { assert(false); }
		virtual void visit(FloatLiteral* node) { assert(false); }
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
int newErrors = 0;

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

void error(const Token& token, string msg)
{
	auto* s = getScanner();
	const uint column = s->currentColumn();
	const uint row = s->currentRow();
	const int tokenLength = std::max((int)token.symbol.length() - 1, 0);

	s_parserErrors.push_back(ParserError { msg, token, column - tokenLength, row });
	newErrors++;
}

void advanceToken()
{
	s_tokens.push_back(s_currentToken);
	getScanner()->getToken(&s_currentToken, nullptr);
	//printLine(string("Found token: ") + toString(s_currentToken));
}

const Token& lastToken()
{
	return s_tokens.back();
}

bool accept(TokenType type)
{
	//printLine(string("Accept: ") + toString(type));	
	if (s_currentToken.type == type)
	{
		advanceToken();
		return true;
	}
	return false;
}

bool expect(TokenType type)
{
	//printLine(string("Expect: ") + toString(type));
	if (accept(type))
		return true;
	
	string errMsg = 
			string("Expected '") + toString(type) + "' but got '" + 
			toString(s_currentToken.type) + "'";
	error(s_currentToken, errMsg);
	advanceToken();	
	return false;
}

bool parseExpression(ScannerFactory* scannerFactory, AST::Call* node)
{
	if (accept(TokenType::StringLiteral))
	{
		auto expr = createNode<AST::StringLiteral>();
		expr->value = lastToken().symbol;
		node->args.push_back(expr);
	}
	else if (accept(TokenType::IntegerLiteral))
	{
		auto expr = createNode<AST::IntegerLiteral>();
		expr->value = lastToken().symbol;
		node->args.push_back(expr);
	}
	else if (accept(TokenType::FloatLiteral))
	{
		auto expr = createNode<AST::FloatLiteral>();
		expr->value = lastToken().symbol;
		node->args.push_back(expr);
	}
	else
	{
		return false;
	}

	return true;
}

bool parseCallParameters(ScannerFactory* scannerFactory, AST::Call* call)
{
	while (parseExpression(scannerFactory, call))
	{
		if (accept(TokenType::Comma))
			continue;
		else
			break;
	}

	return true;
}

string stringSymbolValue(string symbol)
{
	return symbol.substr(1, symbol.length() - 2);
}

// TODO: Rename parseModule
bool parseTopLevel(ScannerFactory* scannerFactory, AST::Module* module)
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
			string symbol = lastToken().symbol;

			// Function call
			if (accept(TokenType::OpenParenthesis))
			{
				auto node = createNode<AST::Call>();
				node->function = symbol;

				parseCallParameters(scannerFactory, node);

				expect(TokenType::CloseParenthesis);
				expect(TokenType::SemiColon);

				module->addStatement(node);					
			}
			else
			{
				error(lastToken(), "No support for non-call symbols yet!");
			}
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
	parseTopLevel(scannerFactory, node);
	expect(TokenType::EndOfScan);

	ast->root = node;
	return s_parserErrors.size() == 0;
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

/*
struct CExpressionGenerator
{
	CExpressionGenerator(std::ostream* out, int indent)
		: m_out(out)
		, m_indent(indent)
	{}

	void visit(AST::Call* node) override
	{
		//return "";
	};

	void visit(AST::StringLiteral* node) override
	{
		auto& out = m_head;

		if (node->type == AST::Import::Type_C)
			out << "#include <" << node->file << ">\n";
			
		//return "";
	};

	std::ostream* m_out;
	int m_indent;
}
*/
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

	void appendArg(std::ostream& out, AST::Node* node)
	{
		if (auto* n = dynamic_cast<AST::StringLiteral*>(node))
		{
			out << n->value;
		}
		else if (auto* n = dynamic_cast<AST::IntegerLiteral*>(node))
		{
			out << n->value;
		}
		else if (auto* n = dynamic_cast<AST::FloatLiteral*>(node))
		{
			out << n->value;
		}
	}

	void visit(AST::Call* node) override
	{
		auto& out = m_body;
		/*CExpressionGenerator exprGen(out, m_indent);

		for (auto a : node->args)
		{
			string s = a->accept(exprGen);
			print(s);
		}*/


		out << node->function << "(";
		int argCount = node->args.size();
		if (argCount > 0)
		{
			appendArg(out, node->args[0]);
			for (int i = 1; i < argCount; ++i)
			{
				out << ", ";
				appendArg(out, node->args[i]);
			}
		}
		out << ");\n";
	};

	std::ostream* m_out;
	std::stringstream m_head;
	std::stringstream m_body;
	int m_indent;
}; 


std::ifstream& gotoLine(std::ifstream& file, unsigned int num){
    file.seekg(std::ios::beg);
    for(int i=0; i < num - 1; ++i){
        file.ignore(std::numeric_limits<std::streamsize>::max(),'\n');
    }
    return file;
}

void printPointAtColumn(uint column, int indent = 0)
{
	indent = ((column - 1) / 4) + 1;
	printIndent(indent);
	uint rest = (column - 1) % 4;
	for (int i = 0; i < rest; ++i)
		print(" ");
	printLine("\033[1;31m^\033[0m");
}

void printParserErrors(std::ifstream& file)
{
	int hej;
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
	ParseError error;

	LOG("Parsing...");
	if (!parse(&scannerFactory, &ast))
	{
		LOG("Parse fail!");
		auto f = std::ifstream(args[0]);
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

