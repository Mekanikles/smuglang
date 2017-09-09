#pragma once

struct CGenerator : AST::Visitor
{
	CGenerator(std::ostream* out)
		: m_out(out)
		, m_indent(0)
	{}

	void run(AST* ast)
	{
		ast->root->accept(this);

		auto& out = *m_out;
	
		out << m_head.str();
		out << m_body.str();
	}

	void visit(AST::Module* node) override
	{
		auto& out = m_body;
		out << "int main()\n{\n";
		{
			m_indent++;
			for (auto s : node->statements)
			{
				s->accept(this);
			}
		}
		out << "}\n";
	}

	void visit(AST::Import* node) override
	{
		auto& out = m_head;

		if (node->type == AST::Import::Type_C)
			out << "#include <" << node->file << ">\n";
	}

	void visit(AST::Call* node) override
	{
		auto& out = m_body;

		out << indent(m_indent) << node->function << "(";
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
		auto& out = m_body;

		out << "(";
		node->left->accept(this);
		out << " ";
		switch (node->type)
		{
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
		auto& out = m_body;

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
		auto& out = m_body;

		node->expr->accept(this);

		switch (node->type)
		{
			case TokenType::IncrementOp: out << "++ "; break;
			case TokenType::DecrementOp: out << "-- "; break;
			default: assert(false);
		}
	}

	void visit(AST::StringLiteral* node) override
	{	m_body << node->value; }

	void visit(AST::IntegerLiteral* node) override
	{	m_body << node->value; }

	void visit(AST::FloatLiteral* node) override
	{	m_body << node->value; }

	std::ostream* m_out;
	std::stringstream m_head;
	std::stringstream m_body;
	int m_indent;
	unsigned m_tempVarCount = 0;
}; 

