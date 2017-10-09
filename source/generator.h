#pragma once

struct Output
{
	std::ostream* imports;
	std::ostream* data;
	std::ostream* body;
};

struct SymbolTranslation
{
	Symbol* symbol;
	string translation;
};

// TODO: This needs to be scoped to the same scope as symbol
//	Add data* to symbol for this instead?
vector<SymbolTranslation> s_functionTranslations;

const string& translateFunctionSymbol(const Symbol* symbol)
{
	for (auto& e : s_functionTranslations)
	{
		if (e.symbol == symbol)
			return e.translation;
	}
	return symbol->name;
}

struct BodyGenerator : AST::Visitor
{
	BodyGenerator(const Output& out, int indent)
		: m_out(out)
		, m_indent(indent)
	{}

	void visit(AST::StatementBody* node) override
	{
		auto& out = *m_out.body;

		out << indent(m_indent) << "{\n";
		m_indent++;

		// Declare all symbols in scope
		for (Symbol* s : node->scope.symbols)
		{
			// params are already declared in the signature
			if (s->isParam)
				continue;

			if (s->isFunction)
			{
				AST::FunctionDeclaration* declNode = (AST::FunctionDeclaration*)s->declNode;

				if (declNode->funcLiteral)
				{
					const string name = string("") + declNode->symbol + "_" + std::to_string(declNode->order);
					generateFunctionLiteral(declNode->funcLiteral, name);
					s_functionTranslations.push_back(SymbolTranslation{declNode->symbolObj, name});
				}
				else
				{
					// TODO: Handle duplicate externals
					//*m_out.data << "extern int " << declNode->symbolObj->name << "();\n";
				}
			}
			else
			{
				// TODO: Unsafe cast
				AST::SymbolDeclaration* declNode = (AST::SymbolDeclaration*)s->declNode;
				assert(declNode);

				// TODO: Store all info in symbol, handle inferred types
				if (declNode->typeExpr)
				{
					out << indent(m_indent);
					declNode->typeExpr->accept(this);
					out << " " << declNode->symbol;
					out << ";\n";
				}
			}
		}

		for (auto s : node->statements)
		{
			s->accept(this);
		}

		m_indent--;
		out << indent(m_indent) << "}\n";
	}

	void visit(AST::Import* node) override
	{
		auto& out = *m_out.imports;

		if (node->type == AST::Import::Type_C)
			out << "#include <" << node->file << ">\n";
	}

	void visit(AST::Call* node) override
	{
		auto& out = *m_out.body;

		assert(node->expr);
		const string& name = translateFunctionSymbol(node->expr->symbolObj);

		out << indent(m_indent) << name << "(";
		int argCount = node->args.size();
		if (argCount > 0)
		{
			node->args[0]->accept(this);
			for (int i = 1; i < argCount; ++i)
			{
				out << ", ";
				node->args[i]->accept(this);
			}
		}
		out << ");\n";
	};

	void visit(AST::BinaryOp* node) override
	{	
		auto& out = *m_out.body;

		out << "(";
		node->left->accept(this);
		out << " ";
		switch (node->type)
		{
			case TokenType::CompareOp: out << "== "; break;
			case TokenType::AddOp: out << "+ "; break;
			case TokenType::SubtractOp: out << "- "; break;
			case TokenType::MultiplicationOp: out << "* "; break;
			case TokenType::DivisionOp: out << "/ "; break;		
			default: assert(false);
		}
		node->right->accept(this);
		out << ")";
	}

	void visit(AST::UnaryOp* node) override
	{
		auto& out = *m_out.body;

		switch (node->type)
		{
			case TokenType::AddOp: out << "+ "; break;
			case TokenType::SubtractOp: out << "- "; break;
			case TokenType::IncrementOp: out << "++ "; break;
			case TokenType::DecrementOp: out << "-- "; break;
			default: assert(false);
		}
		node->expr->accept(this);
	}

	void visit(AST::UnaryPostfixOp* node) override
	{
		auto& out = *m_out.body;

		node->expr->accept(this);

		switch (node->type)
		{
			case TokenType::IncrementOp: out << "++ "; break;
			case TokenType::DecrementOp: out << "-- "; break;
			default: assert(false);
		}
	}

	void visit(AST::SymbolDeclaration* node) override
	{
		auto& out = *m_out.body;

		assert(node->symbolObj);
		Type& t = node->symbolObj->type;

		// Declaration has already been done in body
		//	only initialization has to be generated
		// Params will not be initialized inside function body
		//	but instead by caller
		// Static const functions does initialization on load instead
		//	TODO: Generalize
		if (!node->isParam && !t.isFunction && node->initExpr)
		{
			out << indent(m_indent);
			out << node->symbol;
			out << " = ";
			node->initExpr->accept(this);
			out << ";\n"; 
		}
		else
		{
			node->initExpr->accept(this);
		}
	}

	void visit(AST::SymbolExpression* node) override
	{
		auto& out = *m_out.body;
		out << node->symbol; 
	}

	void visit(AST::StringLiteral* node) override
	{	
		auto& out = *m_out.body;
		out << node->value; 
	}

	void visit(AST::IntegerLiteral* node) override
	{
		auto& out = *m_out.body;
		out << node->value; 
	}

	void visit(AST::FloatLiteral* node) override
	{	
		auto& out = *m_out.body;
		out << node->value; 
	}

	string generateFunctionLiteral(AST::FunctionLiteral* node, const string& name)
	{
		std::stringstream imports;
		std::stringstream data;
		std::stringstream body;
		Output output { &imports, &data, &body };
		BodyGenerator bodyGenerator(output, 0);

		auto& out = body;

		// TODO: Add support for out params
		out << "void " << name << "(";

		int size = node->signature->params.size();
		for (int i = 0; i < size; ++i)
		{
			AST::SymbolDeclaration* decl = node->signature->params[i];
			// TODO: By this point, we should not need to evaluate exprs for types/symbols
			//	we should be able to look to symbol/type tables
			if (decl->typeExpr)
				decl->typeExpr->accept(&bodyGenerator);
			out << " " << decl->symbol;
			if (decl->initExpr)
				decl->initExpr->accept(&bodyGenerator);

			if (i < size - 1)
				out << ", ";
		}

		out << ")\n";

		// Generate body
		node->body->accept(&bodyGenerator);

		*m_out.imports << imports.str();
		*m_out.data << data.str();
		*m_out.data << body.str();

		return name;
	}

	void visit(AST::FunctionLiteral* node) override
	{	
		static int count = 0;
		auto& out = *m_out.body;
		const string name = string("__lambda") + std::to_string(count++);
		out << generateFunctionLiteral(node, name); 
	}

	void visit(AST::FunctionDeclaration* node) override
	{	
		// Do nothing, function literal has already been visited by body
	}

	void visit(AST::IfStatement* node) override
	{	
		auto& out = *m_out.body;

		assert(node->expr);

		out << indent(m_indent) << "if ";

		node->expr->accept(this);

		out << "\n";

		std::stringstream imports;
		std::stringstream data;
		std::stringstream body;
		Output output { &imports, &data, m_out.body };

		{
			bool isBody = dynamic_cast<AST::StatementBody*>(node->statement) != nullptr;
			BodyGenerator bodyGenerator(output, isBody ? m_indent : m_indent + 1);
			node->statement->accept(&bodyGenerator);
		}

		if (node->elseStatement)
		{	
			out << indent(m_indent) << "else\n";
			bool isBody = dynamic_cast<AST::StatementBody*>(node->elseStatement) != nullptr;
			BodyGenerator bodyGenerator(output, isBody ? m_indent : m_indent + 1);		
			node->elseStatement->accept(&bodyGenerator);
		}

		*m_out.imports << imports.str();
		*m_out.data << data.str();
	}	

	Output m_out;
	int m_indent;
};

struct CGenerator : AST::Visitor
{
	CGenerator(std::ostream* out)
		: m_out(out)
		, m_indent(0)
	{}

	void run(AST::AST* ast)
	{
		ast->root->accept(this);

		auto& out = *m_out;
	
		out << m_imports.str();
		out << m_data.str();
		out << m_body.str();
	}

	void visit(AST::Module* node) override
	{
		auto& out = m_body;
		out << "int main()\n";
		{
			Output output { &m_imports, &m_data, &m_body };
			BodyGenerator bodyGenerator(output, m_indent);
			node->body->accept(&bodyGenerator);
		}
	}

	std::ostream* m_out;
	std::stringstream m_imports;
	std::stringstream m_data;
	std::stringstream m_body;
	int m_indent;
	unsigned m_tempVarCount = 0;
}; 

