#pragma once

#include <iostream>
#include <sstream>
#include <regex>
#include <limits>

#include <fstream>
#include <cctype>

#include <cassert>
#include <cstdlib>
#include <vector>
#include <string>
#include <utility>

#include <experimental/optional>
namespace std
{
	template<typename T>
	using optional = experimental::optional<T>;
}

template<typename T>
using vector = std::vector<T>;
using string = std::string;
template<typename T>
using unique = std::unique_ptr<T>;

using uint = unsigned int;
using u8 = unsigned char;
using u64 = unsigned long;

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

template<typename T>
struct ObjectStore
{
	static vector<T*> s_instances;
};

template<typename T>
vector<T*> ObjectStore<T>::s_instances = {};

template<typename T, typename... Args>
T* createObject(Args... args)
{
	T* o = new T(std::forward<Args>(args)...);
	ObjectStore<T>::s_instances.push_back(o);
	return o;
}

enum class FGTextColor
{
	Unchanged = 0,
	Black = 30,
	Red = 31,
	Green = 32,
	Yellow = 33,
	Blue = 34,
	Magenta = 35,
	Cyan = 36,
	White = 37,
	BrightBlack = 90,
	BrightRed = 91,
	BrightGreen = 92,
	BrightYellow = 93,
	BrightBlue = 94,
	BrightMagenta = 95,
	BrightCyan = 96,
	BrightWhite = 97
};

enum class BGTextColor
{
	Unchanged = 0,	
	Black = 40,
	Red = 41,
	Green = 42,
	Yellow = 43,
	Blue = 44,
	Magenta = 45,
	Cyan = 46,
	White = 47,
	BrightBlack = 100,
	BrightRed = 101,
	BrightGreen = 102,
	BrightYellow = 103,
	BrightBlue = 104,
	BrightMagenta = 105,
	BrightCyan = 106,
	BrightWhite = 107
};

string prettyString(string str, FGTextColor fbColor, BGTextColor bgColor, bool bold)
{
	string ret = string("\033[");
	bool insertSemiColon = false;
	if (fbColor != FGTextColor::Unchanged)
	{
		ret += std::to_string((int)fbColor);
		insertSemiColon = true;
	}
	if (bgColor != BGTextColor::Unchanged)
	{
		if (insertSemiColon)
			ret += ";";
		ret += std::to_string((int)bgColor);
		insertSemiColon = true;
	}
	if (bold)
	{
		if (insertSemiColon)
			ret += ";";
		ret += "1";
	}

	ret += string("m");
	ret += str;
	ret += string("\033[0m");
	return ret;
}

string prettyString(string str, FGTextColor fbColor, bool bold = false)
{
	return prettyString(str, fbColor, BGTextColor::Unchanged, bold);
}

string prettyString(string str, bool bold)
{
	return prettyString(str, FGTextColor::Unchanged, BGTextColor::Unchanged, bold);
}






