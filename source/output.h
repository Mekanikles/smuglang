#pragma once

void printAST(AST::AST* ast, int indent = 0)
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

std::istream& gotoLine(std::istream& file, unsigned int num){
    file.seekg(std::ios::beg);
    for(int i=0; i < num; ++i){
        file.ignore(std::numeric_limits<std::streamsize>::max(),'\n');
    }
    return file;
}

void printPointAtColumn(int column, char line[256], int indent)
{
	for (uint c = 0; c < column; ++c)
	{
		if (line[c] == '\t')
			print("\t");
		else
			print(" ");
	}
	printLine("\033[1;31m^\033[0m");
}

int tabcorrectColumn(int column, char line[256])
{
	int outColumn = column;
	for (uint c = 0; c < column; ++c)
	{
		if (line[c] == '\t')
			outColumn += 3; // Tab width - 1 TODO: Have it configurable
	}
	return outColumn;
}

void printScannerErrors(const Parser& parser)
{
	auto filePtr = parser.getSourceInput()->createStream();
	auto& file = *filePtr;
	for (auto e : parser.getScannerErrors())
	{
		// TODO: Make into segment surrounding error instead, to support really long lines
		char line[256];
		auto& fileAtLine = gotoLine(file, e.row);
		fileAtLine.getline(line, 256);

		int correctedColumn = tabcorrectColumn(e.column, line);
		printLine(string("\033[1m") + std::to_string(e.row + 1) + 
				":" + std::to_string(e.column + 1) + ": \033[31mError: \033[39m" + e.msg + "\033[0m");

		printLine(string(line), 0);
		printPointAtColumn(e.column, line, 1);
	}
}

void printParserErrors(const Parser& parser)
{
	auto filePtr = parser.getSourceInput()->createStream();
	auto& file = *filePtr;
	for (auto e : parser.getParserErrors())
	{
		// TODO: Make into segment surrounding error instead, to support really long lines
		char line[256];
		auto& fileAtLine = gotoLine(file, e.row);
		fileAtLine.getline(line, 256);

		int correctedColumn = tabcorrectColumn(e.column, line);
		printLine(string("\033[1m") + std::to_string(e.row + 1) + 
				":" + std::to_string(correctedColumn + 1) + ": \033[31mError: \033[39m" + e.msg + "\033[0m");

		printLine(string(line), 0);
		printPointAtColumn(e.column, line, 1);
	}
}



