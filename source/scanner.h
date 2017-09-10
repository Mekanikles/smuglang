#pragma once

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

			if (n == '=')
			{
				m_inStream.ignore();
				*outToken = Token(TokenType::Equals);
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

			// Symbols and keywords
			if (isalpha(n))
			{
				string w = scanWord();
				// TODO: Difference between toplevel scan and body scan?
				if (w == "import")
					*outToken = Token(TokenType::Import);
				else if (w == "var")
					*outToken = Token(TokenType::Var);
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
