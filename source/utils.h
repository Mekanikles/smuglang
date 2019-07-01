#pragma once

bool checkAndReverseEscapeCharacter(char c, char& e)
{
	if (c == '\"')
		e = '\"';
	else if (c == '\'')
		e = '\'';
	else if (c == '\n')
		e = 'n';
	else if (c == '\r')
		e = 'r';
	else if (c == '\t')
		e = 't';
	else if (c == '\0')
		e = '0';
	else if (c == '\b')
		e = 'b';	
	else
		return false;

	return true;
}

string processStringForOutput(string in)
{
	string out;
	for (char c : in)
	{
		char e;
		if (checkAndReverseEscapeCharacter(c, e))
		{
			out += '\\';
			out += e;
			continue;
		}

		out += c;
	}
	return out;
}

static char getEscapeCharacter(char c)
{
	if (c == 'n')
		return '\n';
	else if (c == 'r')
		return '\r';
	else if (c == 't')
		return '\t';
	else if (c == '0')
		return '\0';
	else if (c == 'b')
		return '\b';
	return c;
}

static u8 hexToDec(char c)
{
	if (c >= '0' && c <= '9')
		return c;
	else if (c >= 'A' && c <= 'F')
		return (c - 'A') + 10;
	assert(false && "Invalid hex character");
	return 0; 
}

string processQuotedInputString(string in)
{
	string out;
	uint length = in.length();
	const char* buffer = in.data();
	assert(buffer[0] == '\"');
	assert(buffer[length - 1] == '\"');
	const char* cp = &buffer[1];
	while (cp < &buffer[length - 1])
	{
		char c = *cp;
		if (c == '\\' && cp + 1 < &buffer[length - 1])
		{
			cp++;
			if (*cp == 'x' && cp + 2 < &buffer[length - 1])
			{
				u8 hex = hexToDec(*++cp) * 16;
				hex += hexToDec(*++cp);
				c = hex;
			}
			else
			{
				c = getEscapeCharacter(*cp);
			}
		}

		out += c;
		cp++;
	}
	return out;
}


