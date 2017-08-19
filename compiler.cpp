#include <cassert>
#include <vector>
#include <string>
#include <iostream>
#include <sstream>
#include <regex>

#include <fstream>
#include <cctype>

template<typename T>
using vector = std::vector<T>;
using string = std::string;

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

string toString(const Token& t)
{
	switch (t.type)
	{
		case TokenType::Import: return "Import";
		case TokenType::OpenParenthesis: return "OpenParenthesis";
		case TokenType::CloseParenthesis: return "CloseParenthesis";
		case TokenType::StringLiteral: return string("StringLiteral(") + t.symbol + ")";
		case TokenType::IntegerLiteral: return string("IntegerLiteral(") + t.symbol + ")";
		case TokenType::FloatLiteral: return string("FloatLiteral(") + t.symbol + ")";
		case TokenType::Symbol: return string("Symbol(") + t.symbol + ")";
		case TokenType::SemiColon: return string("SemiColon");
		case TokenType::Comma: return string("Comma");
		case TokenType::EndOfScan: return "EndOfScan";
		case TokenType::CompilerDirective: return string("CompilerDirective(") + t.symbol + ")";
		default: return "UknownToken";
	};
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

	char get()
	{
		char c;
		return get(c) ? c : EOF;
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
		const uint buffered = bufferedLength();
		if (count < buffered)
		{
			m_begin += count;
			m_begin = m_begin % BUF_SIZE;
		}
		else
		{
			m_inStream.ignore(count - buffered);
			m_end = m_begin;
		}
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
};

class Scanner
{
public:	
	Scanner(BufferedInputStream& inStream) 
		: m_inStream(inStream)
	{
	}

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

class TopLevelScanner : Scanner
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
		return ret;
	}

	bool getToken(Token* outToken, ParseError* outError)
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

			if (n == '"' || n == '\'')
			{
				string s = parseStringLiteral();
				*outToken = Token(TokenType::StringLiteral, s);
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
	struct Module;
	struct Import;
	struct Call;

	struct Visitor;

	struct Node
	{
		vector<Node*> children;
		void addChild(AST::Node* child)
		{
			children.push_back(child);
		}

		void acceptChildren(Visitor* v)
		{
			for (auto* child : children)
				child->accept(v);
		}

		virtual ~Node() = default;

		virtual void accept(Visitor* v) = 0;
		virtual string toString() = 0;	
	};

	struct Visitor
	{
		virtual void visit(Module* node) { node->acceptChildren(this); }
		virtual void visit(Import* node) { node->acceptChildren(this); }
		virtual void visit(Call* node) { node->acceptChildren(this); }
	};

	template<typename T>
	struct NodeImpl : Node
	{
		void accept(Visitor* v) override { v->visit((T*)this); }
	};

	struct Module : NodeImpl<Module>
	{
		string toString() override { return "Module"; }
	};

	struct Import : NodeImpl<Import>
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

	struct Call : NodeImpl<Call>
	{
		string function;
		vector<string> args;
		string toString() override 
		{ 
			string s = "Call(";
			s += function;
			for (auto& a : args)
				s += string(", ") + a;
			s += ")";
			return s; 
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

vector<Token> s_tokens;

bool parseTopLevel(ScannerFactory* scannerFactory, AST::Node* root, ParseError* outError)
{
	auto s = scannerFactory->scanTopLevel();

	Token token;
	while(1)
	{
		if (s.getToken(&token, outError))
		{
			s_tokens.push_back(token);

			switch (token.type)
			{
				case TokenType::EndOfScan:
					return true;
				case TokenType::Import:
				{
					Token t;
					s.getToken(&t, outError);

					AST::Import::Type importType = AST::Import::Type_Native;

					// optional import params
					// TODO: invoke new parameter list scanner?
					if (t.type == TokenType::OpenParenthesis)
					{
						s_tokens.push_back(t);

						s.getToken(&t, outError);
						s_tokens.push_back(t);
						assert(t.type == TokenType::StringLiteral);

						assert(t.symbol == "c");
						importType = AST::Import::Type_C;

						s.getToken(&t, outError);
						s_tokens.push_back(t);
						assert(t.type == TokenType::CloseParenthesis);

						s.getToken(&t, outError);
					}
				
					s_tokens.push_back(t);
					assert(t.type == TokenType::StringLiteral);
					string file = t.symbol;

					s.getToken(&t, outError);
					s_tokens.push_back(t);
					assert(t.type == TokenType::SemiColon);

					auto node = createNode<AST::Import>();
					node->file = file;
					assert(importType == AST::Import::Type_C);
					node->type = importType;
					root->addChild(node);
					break;
				}
				case TokenType::CompilerDirective:
				{
					break;
				}
				case TokenType::Symbol:
				{
					Token t;
					s.getToken(&t, outError);
					s_tokens.push_back(t);

					// Function call
					if (t.type == TokenType::OpenParenthesis)
					{
						auto node = createNode<AST::Call>();
						node->function = token.symbol;

						s.getToken(&t, outError);

						// TODO: temp
						vector<string> args;

						// TODO: invoke param parser (which invokes expr parser)
						if (t.type == TokenType::StringLiteral ||
							t.type == TokenType::IntegerLiteral ||
							t.type == TokenType::FloatLiteral)
						{
							while(true)
							{
								assert(t.type == TokenType::StringLiteral ||
										t.type == TokenType::IntegerLiteral ||
										t.type == TokenType::FloatLiteral);
								s_tokens.push_back(t);

								if (t.type == TokenType::StringLiteral)
								{
									node->args.push_back(string("\"") + t.symbol + "\"");
								}
								else
								{
									node->args.push_back(t.symbol);
								}

								s.getToken(&t, outError);
								if (t.type == TokenType::Comma)
								{
									s_tokens.push_back(t);
									s.getToken(&t, outError);
									continue;
								}
								else
								{
									break;
								}
							}
						}

						assert(t.type == TokenType::CloseParenthesis);
						s_tokens.push_back(t);
						

						s.getToken(&t, outError);
						assert(t.type == TokenType::SemiColon);
						s_tokens.push_back(t);

						root->addChild(node);					
					}
				}

				default:
					break;
			}
		}
		else
		{
			return false;
		}
	}

	return true;
}


bool parse(ScannerFactory* scannerFactory, AST* ast, ParseError* outError)
{
	assert(!ast->root);	
	ast->root = createNode<AST::Module>();

	return parseTopLevel(scannerFactory, ast->root, outError);
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
			for (int i = 0; depth - 1; ++i)
			{
				if (depthStack[i])
					print("| ");
				else
					print("  ");
			}
			print("|-");
		}

		printLine(node->toString());

		const int childCount = node->children.size();
		if (childCount > 0)
		{
			depthStack.push_back(true);
			for (int i = 0; i < childCount - 1; ++i)
			{
				rec(node->children[i]);
			}
			depthStack.back() = false;
			rec(node->children.back());
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
		out << "int main()\n{\n";
		out << m_body.str();
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
		out << node->function << "(";
		int argCount = node->args.size();
		if (argCount > 0)
		{
			out << node->args[0];
			for (int i = 1; i < argCount; ++i)
				out << ", " << node->args[i];
		}
		out << ");\n";
	};

	std::ostream* m_out;
	std::stringstream m_head;
	std::stringstream m_body;
	int m_indent;
}; 

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

	if (!parse(&scannerFactory, &ast, &error))
	{
		LOG(error.error);
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

