#pragma once

struct ScannerError
{
	string msg;
	uint column;
	uint row;
};

class Scanner
{
public:	
	Scanner(BufferedInputStream& inStream, 
			vector<ScannerError>& scannerErrors,
			int& newScannerErrors)
		: m_inStream(inStream)
		, scannerErrors(scannerErrors)
		, newScannerErrors(newScannerErrors)
	{
	}

	virtual bool getToken(Token* outToken) = 0;

	uint lastTokenColumn() { return m_lastTokenColumn; }
	uint lastTokenRow() { return m_lastTokenRow; }

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
		const uint column = m_inStream.lastColumn();
		const uint row = m_inStream.lastRow();

		this->scannerErrors.push_back(ScannerError { msg, column, row });
		this->newScannerErrors++;
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
	vector<ScannerError>& scannerErrors;
	int& newScannerErrors;	

	uint m_lastTokenColumn = 0;
	uint m_lastTokenRow = 0;		
};

bool isWordInitChar(const char c)
{
	return isalpha(c) || c == '_';
}

bool isWordChar(const char c)
{
	return isalnum(c) || c == '_';
}

class TopLevelScanner : public Scanner
{
public:	
	using Scanner::Scanner;

	string scanWord()
	{
		string ret;
		if (isWordInitChar(m_inStream.peek()))
		{
			char c;
			m_inStream.get(c);
			ret += c;
			while(isWordChar(m_inStream.peek()))
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
			// hexadecimal integer
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

			// binary integer
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
				if (c == '\\' && m_inStream.peek() != EOF)
				{
					ret += c;
					m_inStream.get(c);
				}

				ret += c;
				assert(m_inStream.peek() != EOF);
			}
		}
		else if (c == '\'')
		{
			ret += c;
			// TODO: Add error reporing
			while(m_inStream.peek() != '\'')
			{
				m_inStream.get(c);
				if (c == '\\' && m_inStream.peek() != EOF)
				{
					ret += c;
					m_inStream.get(c);
				}

				ret += c;
				assert(m_inStream.peek() != EOF);
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

			// At this point, we "know" that the following chars will lead
			//	to either a token or a scanner error, save the positions
			//	for token error messaging.
			m_lastTokenColumn = m_inStream.currentColumn();;
			m_lastTokenRow = m_inStream.currentRow();;

			if (n == ';')
			{
				m_inStream.ignore();
				*outToken = Token(TokenType::SemiColon);
				return true;
			}

			if (n == ':')
			{
				m_inStream.ignore();
				*outToken = Token(TokenType::Colon);
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
				else
				{
					return false;
				}
			}

			// Symbols and keywords
			if (isWordInitChar(n))
			{
				string w = scanWord();
				// TODO: Difference between toplevel scan and body scan?
				if (w == "import")
					*outToken = Token(TokenType::Import);
				else if (w == "extern")
					*outToken = Token(TokenType::Extern);
				else if (w == "if")
					*outToken = Token(TokenType::If);
				else if (w == "else")
					*outToken = Token(TokenType::Else);									
				else if (w == "var")
					*outToken = Token(TokenType::Var);
				else if (w == "func")
					*outToken = Token(TokenType::Func);
				else if (w == "eval")
					*outToken = Token(TokenType::Eval);
				else
					*outToken = Token(TokenType::Symbol, w);
				return true;
			}

			// Numericals
			if (isdigit(n) || 
				(n == '.' && isdigit(m_inStream.lookAhead())))
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

			// Note scan lone dot after numericals to allow decimal point
			if (n == '.')
			{
				m_inStream.ignore();
				*outToken = Token(TokenType::Dot);
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
			else if (n == '=' && n2 == '=')
			{
				m_inStream.ignore(2);
				*outToken = Token(TokenType::CompareOp);
				return true;
			}

			// single-digit operators
			if (n == '=')
			{
				m_inStream.ignore();
				*outToken = Token(TokenType::Equals);
				return true;
			}
			else if (n == '+')
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

			if (n == '{')
			{
				m_inStream.ignore();
				*outToken = Token(TokenType::OpenBrace);
				return true;
			}

			if (n == '}')
			{
				m_inStream.ignore();
				*outToken = Token(TokenType::CloseBrace);
				return true;
			}

			// Don't let anything unparsed through
			m_inStream.ignore();			
			error("Unrecognized character");
			return false;
		}

		*outToken =Token(TokenType::EndOfScan);
		return true;
	}
};

class ScannerFactory
{
public:
	ScannerFactory(BufferedInputStream inStream) 
		: m_inStream(std::move(inStream))
	{
	}

	TopLevelScanner scanTopLevel() { return TopLevelScanner(m_inStream, scannerErrors, newScannerErrors); }

	const vector<ScannerError>& getScannerErrors() const { return this->scannerErrors; }

private:
	BufferedInputStream m_inStream;
	vector<ScannerError> scannerErrors;
	int newScannerErrors = 0;
};
