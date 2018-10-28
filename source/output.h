#pragma once

void printAST(Context* context, AST::ASTObject* ast, int indent = 0)
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
		char orderStr[8];
		snprintf(orderStr, 8, "%*u", 4, node->order);
		print(orderStr);
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

		printLine(node->toString(context));

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

void printIRExpression(IR::Expression* expression, int indent = 0)
{
	/*
		Expr
		|-SubExpr1
		| |-SubSubExpr
		|-SubExpr2
	*/

	auto typeString = [](const TypeRef& t)
	{
		if (t->isConcrete())
			return string(", \033[35;1mType: \033[0m\033[35m") + t.toString() + string("\033[0m");
		else
			return string(", \033[91;1mType: \033[0m\033[91m") + t.toString() + string("\033[0m");
	};

	vector<bool> depthStack;
	std::function<void(IR::Expression*)> printRec = [&](IR::Expression* expr)
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

		print(expr->toString());
		printLine(typeString(expr->getType()));

		const auto& subExprs = expr->getSubExpressions();
		if (!subExprs.empty())
		{
			const int exprCount = subExprs.size();

			depthStack.push_back(true);
			for (int i = 0; i < exprCount - 1; ++i)
			{
				printRec(subExprs[i]);
			}
			depthStack.back() = false;
			printRec(subExprs.back());
			depthStack.pop_back();		
		}
	};

	printRec(expression);
}

void printIRStatement(IR::Statement* statement, int indent = 0)
{
	switch (statement->statementType)
	{
	case IR::Statement::Assignment:
	{
		auto* assignment = static_cast<IR::Assignment*>(statement);
		printLine("Assign", indent);
		printLine("<var>", indent + 1);
		printLine("  to:");
		printIRExpression(assignment->expression.get(), indent + 1);
		break;
	}

	case IR::Statement::Call:
	{
		auto* call = static_cast<IR::Call*>(statement);
		printLine("Call", indent);
		break;
	}

	case IR::Statement::Conditional:
	{
		auto* conditional = static_cast<IR::Conditional*>(statement);
		printLine("Conditional", indent);		
		break;
	}
	}
}

void printIRBlock(IR::Block* block, int indent = 0)
{
	for (auto& s : block->statements)
	{
		printLine("  block:", indent);
		printIRStatement(&*s, indent + 1);
	}	
}

void printIRVariable(IR::Variable* variable, int indent = 0)
{
	printLine("Variable", indent);
}

void printIRFunction(IR::Function* function, int indent = 0)
{
	printLine(function->name, indent);
	printLine("  variables:", indent);
	for (auto& v : function->variables)
		printIRVariable(&v, indent + 1);

	for (auto& b : function->blocks)
	{
		if (function->entryBlock == &b)
			printLine("  entry:", indent);
		else
			printLine("  block:", indent);
		printIRBlock(&b, indent + 1);
	}	
}

void printIRModule(IR::Module* module, int indent = 0)
{
	printLine("Module:", indent);
	printLine("  Functions:", indent);
	for (auto& f : module->functions)
	{
		printIRFunction(&f, indent + 1);
	}
	printIRFunction(&module->mainFunction, indent + 1);
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



