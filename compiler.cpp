#include <cassert>
#include <vector>
#include <string>
#include <iostream>
#include <fstream>
#include <cctype>

template<typename T>
using vector = std::vector<T>;
using string = std::string;

void print(const string& str, int indent = 0)
{
	for (int i = 0; i < indent; ++i)
		std::clog << "    ";	
	std::clog << str;
}

void printLine(const string& str, int indent = 0)
{
	for (int i = 0; i < indent; ++i)
		std::clog << "    ";
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
	OpenParantesis,
	CloseParantesis,
	StringLiteral,
	IntegerLiteral,
	FloatLiteral,
	Symbol,
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
		case TokenType::OpenParantesis: return "OpenParantesis";
		case TokenType::CloseParantesis: return "CloseParantesis";
		case TokenType::StringLiteral: return string("StringLiteral(") + t.symbol + ")";
		case TokenType::IntegerLiteral: return string("IntegerLiteral(") + t.symbol + ")";
		case TokenType::FloatLiteral: return string("FloatLiteral(") + t.symbol + ")";
		case TokenType::Symbol: return string("Symbol(") + t.symbol + ")";
		case TokenType::EndOfScan: return "EndOfScan";
		default: return "UknownToken";
	};
}

struct ParseError
{
	string error;
};

class Scanner
{
public:	
	Scanner(std::istream& inStream) 
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


	ScanResult scan(char c, char n, Token* outToken, ParseError* outParseError)
	{
		if (m_isLineComment)
		{
			if (c == '\n')
				m_isLineComment = false;
			return ScanResult::Skip;
		}
		else if (m_blockCommentLevel > 0)
		{
			if (c == '*' && n == '/')
				--m_blockCommentLevel;
			return ScanResult::Skip;		
		}
		else
		{
			if (c == '/' && n == '/')
			{
				m_isLineComment = true;
				m_inStream.ignore(1);

				return ScanResult::Skip;
			}
			if (c == '/' && n == '*')
			{
				++m_blockCommentLevel;
				m_inStream.ignore(1);

				return ScanResult::Skip;
			}

			return ScanResult::Nothing;
		}
	}

protected:
	std::istream& m_inStream;
	bool m_isLineComment = false;
	int m_blockCommentLevel = 0;
};

class TopLevelScanner : Scanner
{
public:	
	using Scanner::Scanner;

	string parseWord(char c)
	{
		string ret;
		if (isalpha(c))
		{
			ret += c;
			while(isalnum(m_inStream.peek()))
			{
				m_inStream.get(c);
				ret += c;
			}
		}

		return ret;
	}

	string parseStringLiteral(char c)
	{
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
		// eat end-of-string identifier
		m_inStream.get(c);
		return ret;
	}

	bool getToken(Token* outToken, ParseError* outError)
	{
		ParseError error;
		char c;
		outToken->type = TokenType::EndOfScan;
		while (m_inStream.get(c))
		{	
			char n = m_inStream.peek();

			auto scanRes = Scanner::scan(c, n, outToken, outError);
			if (scanRes == ScanResult::FoundToken)
				return true;
			else if (scanRes == ScanResult::Error)
				return false;
			else if (scanRes == ScanResult::Skip)
				continue;

			if (c == ' ' || c == '\t' || c == ';' || c == '\n')
				continue;

			if (isalpha(c))
			{
				string w = parseWord(c);
				*outToken = Token(TokenType::Symbol, w);
				return true;
			}

			if (c == '"' || c == '\'')
			{
				string s = parseStringLiteral(c);
				*outToken = Token(TokenType::StringLiteral, s);
				return true;
			}

			if (c == '(')
			{
				*outToken = Token(TokenType::OpenParantesis);
				return true;
			}

			if (c == ')')
			{
				*outToken = Token(TokenType::CloseParantesis);
				return true;
			}
		}

		return true;
	}
};

class ScannerFactory
{
public:
	ScannerFactory(std::istream& inStream) 
		: m_inStream(inStream)
	{
	}

	TopLevelScanner scanTopLevel() { return TopLevelScanner(m_inStream); }

private:
	std::istream& m_inStream;
};

struct AST
{
	struct Node
	{
		vector<Node*> children;
		virtual ~Node() = default;
	};

	struct Module : Node
	{
	};

	struct Import : Node
	{	
		string file;
	};

	struct Call : Node
	{
		string name;
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


bool parseTopLevel(ScannerFactory* scannerFactory, AST* ast, ParseError* outError)
{
	assert(!ast->root);

	ast->root = createNode<AST::Module>();

	auto s = scannerFactory->scanTopLevel();

	vector<Token> tokens;

	Token token;
	while(1)
	{
		if (s.getToken(&token, outError))
		{
			tokens.push_back(std::move(token));
			if (token.type == TokenType::EndOfScan)
				break;
		}
		else
		{
			return false;
		}
	}

	printLine("Tokens:");
	for (auto& t : tokens)
		printLine(toString(t), 1);

	return true;
}

int main(int argc, char** argv)
{
	vector<string> args(argv + 1, argv + argc);

	if (args.size() < 1)
		ERROR("No input files specified");

	std::ifstream inFile(args[0]);

	ScannerFactory scannerFactory(inFile);
	AST ast;
	ParseError error;

	if (!parseTopLevel(&scannerFactory, &ast, &error))
	{
		LOG(error.error);
	}
	else
	{
		LOG("Parse success!");
	}
}



