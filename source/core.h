#pragma once

#include <cassert>
#include <cstdlib>
#include <vector>
#include <string>

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






