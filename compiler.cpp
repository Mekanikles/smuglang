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

void printIndent(int indent)
{
	for (int i = 0; i < indent; ++i)
		std::clog << "    ";	
}

void print(const string& str, int indent = 0)
{
	printIndent(indent);
	std::clog << str;
}

void printLine(const string& str, int indent = 0)
{
	printIndent(indent);
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
		case TokenType::EndOfScan: return "EndOfScan";
		case TokenType::CompilerDirective: return string("CompilerDirective(") + t.symbol + ")";
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

			if (c == '#')
			{
				m_inStream.get(c);
				string w = parseWord(c);
				// TODO: Add error for no symbol
				if (w != "")
				{
					*outToken = Token(TokenType::CompilerDirective, w);
					return true;
				}
			}

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
				*outToken = Token(TokenType::OpenParenthesis);
				return true;
			}

			if (c == ')')
			{
				*outToken = Token(TokenType::CloseParenthesis);
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
				case TokenType::CompilerDirective:
				{
					if (token.symbol == "import")
					{
						Token t;
						s.getToken(&t, outError);
						s_tokens.push_back(t);
						assert(t.type == TokenType::OpenParenthesis);

						s.getToken(&t, outError);
						s_tokens.push_back(t);
						assert(t.type == TokenType::StringLiteral);

						s.getToken(&t, outError);
						s_tokens.push_back(t);
						assert(t.type == TokenType::CloseParenthesis);

						s.getToken(&t, outError);
						s_tokens.push_back(t);
						assert(t.type == TokenType::StringLiteral);

						auto node = createNode<AST::Import>();
						node->file = t.symbol;
						root->addChild(node);
					}

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
						s_tokens.push_back(t);
						assert(t.type == TokenType::StringLiteral);
						
						node->args.push_back(string("\"") + t.symbol + "\"");

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

struct CGenerator : AST::Visitor
{
	CGenerator(std::ostream* out)
		: m_out(out)
	{}

	void run(AST* ast)
	{
		ast->root->accept(this);

		auto& out = *m_out;
	
		out << m_head.str();
		out << "main(){\n";
		out << m_body.str();
		out << "}\n";
	}

	void visit(AST::Import* node) override
	{
		auto& out = m_head;
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
		out << ");";
	};

	std::ostream* m_out;
	std::stringstream m_head;
	std::stringstream m_body;
}; 

int main(int argc, char** argv)
{
	vector<string> args(argv + 1, argv + argc);

	if (args.size() < 1)
		ERROR("No input files specified");

	std::ifstream inFile(args[0]);

	ScannerFactory scannerFactory(inFile);
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
			for (auto& t : s_tokens)
				printLine(toString(t), 1);

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

