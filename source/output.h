#pragma once

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

std::ifstream& gotoLine(std::ifstream& file, unsigned int num){
    file.seekg(std::ios::beg);
    for(int i=0; i < num - 1; ++i){
        file.ignore(std::numeric_limits<std::streamsize>::max(),'\n');
    }
    return file;
}

void printPointAtColumn(int column, int indent = 0)
{
	indent = ((column - 1) / 4) + 1;
	printIndent(indent);
	int rest = std::max((column - 1) % 4, 0);
	for (int i = 0; i < rest; ++i)
		print(" ");
	printLine("\033[1;31m^\033[0m");
}

void printScannerErrors(std::ifstream& file)
{
	for (auto e : s_scannerErrors)
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

void printParserErrors(std::ifstream& file)
{
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
