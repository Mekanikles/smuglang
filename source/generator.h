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

string getIntegerTypeName(const PrimitiveClass& p)
{
	assert(p.isInteger());
	uint size = p.knownSize ? p.size : DEFAULT_INT_SIZE;
	bool isSigned = p.knowsSign() ? p.isSigned() : DEFAULT_INT_ISSIGNED;

	if (size == 32)
		return isSigned ? "int32_t" : "uint32_t";
	else if (size == 64)
		return isSigned ? "int64_t" : "uint64_t";
	else if (size == 16)
		return isSigned ? "int16_t" : "uint16_t";
	else if (size == 8)
		return isSigned ? "int8_t" : "uint8_t";
	else
		assert("Unsupported integer size" && false);

	return "";
};

string getTypeClassName(const TypeClass* typeClass)
{	
	switch (typeClass->type)
	{
		case TypeClass::Primitive:
		{
			auto& p = typeClass->as<PrimitiveClass>();
			return getIntegerTypeName(p);
		}
		case TypeClass::Pointer:
		{
			auto& p = typeClass->as<PointerClass>();
			string ret = getTypeClassName(p.type.typeClass.get());
			ret += "*";
			return ret;
		}
		default:
			assert("Generator does not support type" && false);
	};
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

		vector<AST::FunctionDeclaration*> functionDeclarations;

		// Declare all symbols in scope
		assert(!node->scope.hasCatchAlls());
		for (auto* ss : node->scope.getDeclarations())
		{
			const bool isExternal = ss->isExternal();
			Symbol* symbol = ss->getSymbol();

			// params are already declared in the signature
			if (symbol->isParam)
				continue;

			const Type& type = symbol->type;

			if (type.isFunction())
			{
				if (isExternal)
				{
					const string name = symbol->name;
					generateFunctionPrototype(type.getFunction(), name, m_out.imports);
					*m_out.imports << ";\n";
					s_functionTranslations.push_back(SymbolTranslation{symbol, name});
				}
				else
				{
					// Add forward declarations for all functions in scope
					// This also triggers adding function translations
					AST::FunctionDeclaration* declNode = (AST::FunctionDeclaration*)symbol->declNode;

					assert(declNode->funcLiteral);
					const string name = string("") + declNode->symbol + "_" + std::to_string(declNode->order);
					generateFunctionSignature(declNode->funcLiteral, name, m_out.imports);
					*m_out.imports << ";\n";
					s_functionTranslations.push_back(SymbolTranslation{declNode->getSymbol(), name});
					functionDeclarations.push_back(declNode);
				}
			}
			else
			{
				// Type variables are not allowed to exist in generation step
				assert(!type.isTypeVariable());
				auto typeClass = type.typeClass.get();

				assert(symbol->declNode);
				const uint declOrder = symbol->declNode->order;

				out << indent(m_indent);
				out << getTypeClassName(typeClass);
				out << " " << symbol->name << "_" << declOrder << ";\n";
			}
		}

		// Generate all functions in scope
		for (AST::FunctionDeclaration* declNode : functionDeclarations)
		{
			const string name = translateFunctionSymbol(declNode->getSymbol());
			generateFunctionLiteral(declNode->funcLiteral, name);
			*m_out.data << "\n";
		}

		for (auto s : node->statements)
		{
			const bool isBody = dynamic_cast<AST::StatementBody*>(s) != nullptr;
			if (!isBody)
				out << indent(m_indent);	

			s->accept(this);
			out << ";\n";
		}

		m_indent--;
		out << indent(m_indent) << "}";
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
		assert(node->expr->dependency);
		Symbol* s = node->expr->dependency->getSymbol();
		const string& name = translateFunctionSymbol(s);

		out << name << "(";
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
		out << ")";
	};

	void visit(AST::Assignment* node) override
	{
		auto& out = *m_out.body;
		
		assert(node->symExpr);
		assert(node->symExpr->dependency);
		Symbol* s = node->symExpr->dependency->getSymbol();
		const string& name = s->name;
		assert(s->declNode);
		const uint declOrder = s->declNode->order;

		out << name << "_" << std::to_string(declOrder) << " = ";

		assert(node->expr);
		node->expr->accept(this);
	}

	void visit(AST::BinaryOp* node) override
	{	
		auto& out = *m_out.body;

		out << "(";
		node->left->accept(this);
		out << " ";
		switch (node->opType)
		{
			case TokenType::CompareOp: out << "== "; break;
			case TokenType::Plus: out << "+ "; break;
			case TokenType::Minus: out << "- "; break;
			case TokenType::Asterisk: out << "* "; break;
			case TokenType::Slash: out << "/ "; break;		
			default: assert(false);
		}
		node->right->accept(this);
		out << ")";
	}

	void visit(AST::UnaryOp* node) override
	{
		auto& out = *m_out.body;

		out << "(";
		switch (node->opType)
		{
			case TokenType::Plus: out << "+"; break;
			case TokenType::Minus: out << "-"; break;
			case TokenType::IncrementOp: out << "++"; break;
			case TokenType::DecrementOp: out << "--"; break;
			default: assert(false);
		}
		node->expr->accept(this);
		out << ")";
	}

	void visit(AST::UnaryPostfixOp* node) override
	{
		auto& out = *m_out.body;

		out << "(";
		node->expr->accept(this);

		switch (node->opType)
		{
			case TokenType::IncrementOp: out << "++"; break;
			case TokenType::DecrementOp: out << "--"; break;
			case TokenType::Asterisk: out << "*"; break;
			case TokenType::Ampersand: out << "&"; break;
			default: assert(false);
		}
		out << ")";
	}

	void visit(AST::SymbolDeclaration* node) override
	{
		auto& out = *m_out.body;

		assert(node->symbolObj);
		const Type& t = node->symbolObj->type;
		const string& name = node->symbolObj->name;
		assert(node->symbolObj->declNode == node);
		const uint declOrder = node->order;

		// Declaration has already been done in body
		//	only initialization has to be generated
		// Params will not be initialized inside function body
		//	but instead by caller
		// Static const functions does initialization on load instead
		//	TODO: Generalize
		if (!node->isParam && !t.isFunction() && node->initExpr)
		{		
			out << name << "_" << declOrder;
			out << " = ";
			node->initExpr->accept(this);
		}
		else if (node->initExpr)
		{
			node->initExpr->accept(this);
		}
	}

	void visit(AST::SymbolExpression* node) override
	{
		auto& out = *m_out.body;
		const string& name = node->getSymbol()->name;
		assert(node->getSymbol()->declNode);
		const uint declOrder = node->getSymbol()->declNode->order;
		out << name << "_" << declOrder; 
	}

	void visit(AST::StringLiteral* node) override
	{	
		auto& out = *m_out.body;
		// TODO: Store processed value in ast
		string val = processQuotedInputString(node->value);
		string str = processStringForOutput(val);
		out << '\"' << str << '\"'; 
	}

	void visit(AST::IntegerLiteral* node) override
	{
		auto& out = *m_out.body;
		const Type& t = node->getType();
		assert(t.isPrimitive());
		
		out <<  node->value; 
	}

	void visit(AST::FloatLiteral* node) override
	{	
		auto& out = *m_out.body;
		out << node->value; 
	}

	void generateFunctionPrototype(const FunctionClass& functionClass, const string& name, std::ostream* out)
	{
		// TODO: Add support for out params
		*out << "void " << name << "(";

		int size = functionClass.inTypes.size();
		for (int i = 0; i < size; ++i)
		{
			const Type& param = functionClass.inTypes[i];
			assert(!param.isTypeVariable());
			auto typeClass = param.typeClass.get();

			*out << getTypeClassName(typeClass);
			if (i < size - 1)
				*out << ", ";
		}

		if (functionClass.isVariadic)
			*out << ", ...";

		*out << ")";		
	}

	void generateFunctionSignature(AST::FunctionLiteral* node, const string& name, std::ostream* out)
	{
		// TODO: Add support for out params
		*out << "void " << name << "(";

		Output output { out, out, out }; // TODO: bleh
		BodyGenerator bodyGenerator(output, 0);

		int size = node->signature->inParams.size();
		for (int i = 0; i < size; ++i)
		{
			AST::FunctionInParam* inParam = node->signature->inParams[i];

			Type& type = inParam->getType();
			assert(!type.isTypeVariable());
			auto typeClass = type.typeClass.get();

			const string& name = inParam->name;
			const uint declOrder = inParam->order;

			*out << getTypeClassName(typeClass);
			*out << " " << name << "_" << declOrder;
			if (inParam->initExpr)
				inParam->initExpr->accept(&bodyGenerator);

			if (i < size - 1)
				*out << ", ";
		}

		*out << ")";
	}

	void generateFunctionBody(AST::FunctionLiteral* node, Output& output)
	{
		BodyGenerator bodyGenerator(output, 0);

		// Generate body
		node->body->accept(&bodyGenerator);
	}

	void generateFunctionLiteral(AST::FunctionLiteral* node, const string& name)
	{
		std::stringstream imports;
		std::stringstream data;
		std::stringstream body;
		Output output { &imports, &data, &body };

		generateFunctionSignature(node, name, &body);
		body << "\n";
		generateFunctionBody(node, output);

		*m_out.imports << imports.str();
		*m_out.data << data.str();
		*m_out.data << body.str();
	}

	void visit(AST::FunctionLiteral* node) override
	{	
		// TODO: Lambdas
		/*static int count = 0;
		auto& out = *m_out.body;
		const string name = string("__lambda") + std::to_string(count++);
		out << generateFunctionLiteral(node, name); */
	}

	void visit(AST::FunctionDeclaration* node) override
	{	
		// Do nothing, function literal has already been visited by body
	}

	void visit(AST::IfStatement* node) override
	{	
		auto& out = *m_out.body;

		assert(node->expr);

		out << "if (";

		node->expr->accept(this);

		out << ")\n";

		std::stringstream imports;
		std::stringstream data;
		std::stringstream body;
		Output output { &imports, &data, m_out.body };

		{
			const bool isBody = dynamic_cast<AST::StatementBody*>(node->statement) != nullptr;
			if (!isBody)
				out << indent(m_indent + 1);
			BodyGenerator bodyGenerator(output, isBody ? m_indent : m_indent + 1);
			node->statement->accept(&bodyGenerator);
			if (!isBody)
				out << ";";
		}

		if (node->elseStatement)
		{	
			out << "\n" << indent(m_indent) << "else";
			const bool isElseIf = dynamic_cast<AST::IfStatement*>(node->elseStatement) != nullptr;
			if (isElseIf)
			{
				out << " ";
			}
			else
			{
				out << "\n";

				const bool isBody = dynamic_cast<AST::StatementBody*>(node->elseStatement) != nullptr;
				if (!isBody)
					out << indent(m_indent + 1);			
			}

			BodyGenerator bodyGenerator(output, m_indent);		
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
	
		out << "#include <stdint.h>\n";
		out << "// Declaration section\n";
		out << m_imports.str();
		out << "\n// Data section\n";
		out << m_data.str();
		out << "\n// Body section\n";
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

