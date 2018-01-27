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

string processQuotedInputString(string in)
{
	string out;
	uint length = in.length();
	assert(*in.begin() == '\"');
	assert(*(in.end() - 1) == '\"');
	char* cp = &in.begin()[1];
	while (cp != &in.begin()[length - 1])
	{
		char c = *cp;
		if (c == '\\' && cp + 1 != &in.begin()[length - 1])
		{
			cp++;
			c = getEscapeCharacter(*cp);
		}

		out += c;
		cp++;
	}
	return out;
}


