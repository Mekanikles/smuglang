#pragma once

namespace AST
{
	struct Node;
	struct Statement;
	struct Declaration;
	struct Expression;
	struct Module;
	struct Import;
	struct Call;
	struct IfStatement;
	struct SymbolDeclaration;
	struct FunctionDeclaration;
	struct SymbolExpression;
	struct StringLiteral;
	struct IntegerLiteral;
	struct FloatLiteral;
	struct FunctionLiteral;
	struct UnaryOp;
	struct UnaryPostfixOp;
	struct BinaryOp;

	struct FuncLiteralSignature;
	struct StatementBody;

	struct Visitor;
	void visitChildren(Declaration* node, Visitor* v);
	void visitChildren(Statement* node, Visitor* v);
	void visitChildren(Module* node, Visitor* v);
	void visitChildren(Expression* node, Visitor* v);

	void visitChildren(StatementBody* node, Visitor* v);
	void visitChildren(FuncLiteralSignature* node, Visitor* v);

	struct Visitor
	{
		virtual void visit(Declaration* node) { visitChildren(node, this); }
		virtual void visit(Statement* node) { visitChildren(node, this); }
		virtual void visit(Module* node) { visitChildren(node, this); }
		virtual void visit(Import* node) { visit((Statement*)node); }
		virtual void visit(Call* node) { visit((Statement*)node); }
		virtual void visit(AST::IfStatement* node) { visit((Statement*)node); }
		virtual void visit(SymbolDeclaration* node) { visit((Statement*)node);}	
		virtual void visit(FunctionDeclaration* node) { visit((Statement*)node);}	
		virtual void visit(Expression* node) { visitChildren(node, this); }
		virtual void visit(SymbolExpression* node) { visit((Expression*)node);}
		virtual void visit(StringLiteral* node) { visit((Expression*)node);}
		virtual void visit(IntegerLiteral* node) { visit((Expression*)node); }
		virtual void visit(FloatLiteral* node) { visit((Expression*)node); }
		virtual void visit(FunctionLiteral* node) { visit((Expression*)node); }
		virtual void visit(UnaryOp* node) { visit((Expression*)node); }
		virtual void visit(UnaryPostfixOp* node) { visit((Expression*)node); }
		virtual void visit(BinaryOp* node) { visit((Expression*)node); }

		virtual void visit(StatementBody* node) { visitChildren(node, this); }
		virtual void visit(FuncLiteralSignature* node) { visitChildren(node, this); }
	};

	static int s_nodeCount = 0;

	struct Node
	{
		virtual ~Node() = default;

		virtual const vector<Node*> getChildren() { return vector<Node*>(); }
		virtual void accept(Visitor* v) = 0;
		virtual string toString() = 0;

		int order = s_nodeCount++;
	};

	void visitChildren(Node* node, Visitor* v) { for (auto n : node->getChildren()) n->accept(v); }

	struct Statement : Node
	{};

	void visitChildren(Statement* node, Visitor* v) { for (auto n : node->getChildren()) n->accept(v); }

	struct Declaration : Statement
	{};

	void visitChildren(Declaration* node, Visitor* v) { for (auto n : node->getChildren()) n->accept(v); }

	struct Expression : Node
	{
		virtual Type getType() { return Type(); };
	};

	void visitChildren(Expression* node, Visitor* v) { for (auto n : node->getChildren()) n->accept(v); }

	template<typename T, typename P = Node>
	struct NodeImpl : P
	{
		void accept(Visitor* v) override { v->visit((T*)this); }
	};

	struct StatementBody : public NodeImpl<StatementBody, Statement>
	{
		SymbolScope scope;
		vector<Statement*> statements;

		string toString() override { return "StatementBody"; }
		const vector<Node*> getChildren() override
		{
			return vector<Node*>(statements.begin(), statements.end());
		}

		void addStatement(Statement* s)
		{
			statements.push_back(s);
		}	
	};

	void visitChildren(StatementBody* node, Visitor* v) { for (auto n : node->getChildren()) n->accept(v); }

	struct Module : public NodeImpl<Module>
	{
		StatementBody* body;

		string toString() override { return "StatementBody"; }
		const vector<Node*> getChildren() override
		{
			assert(body);
			auto ret = vector<Node*>();
			ret.push_back(body);
			return ret;
		}
	};

	void visitChildren(Module* node, Visitor* v) { for (auto n : node->getChildren()) n->accept(v); }

	struct Assignment : public NodeImpl<Assignment, Statement>
	{
		string symbol;
		Expression* expr = nullptr;
		Symbol* symbolObj = nullptr;

		string toString() override 
		{ 
			string s = "Assigment(" + symbol + ")";
			return s; 
		}

		const vector<Node*> getChildren() override
		{
			vector<Node*> ret;
			ret.reserve(1);
			ret.push_back(expr);
			return ret;
		}	
	};

	struct IfStatement : public NodeImpl<IfStatement, Statement>
	{
		Expression* expr = nullptr;
		Statement* statement;
		Statement* elseStatement;

		string toString() override 
		{ 
			string s = "IfStatement";
			return s; 
		}

		const vector<Node*> getChildren() override
		{
			vector<Node*> ret;
			ret.reserve(3);
			assert(expr);
			assert(statement);
			ret.push_back(expr);
			ret.push_back(statement);
			if (elseStatement)
				ret.push_back(elseStatement);

			return ret;
		}	
	};

	struct Import : public NodeImpl<Import, Statement>
	{	
		enum Type
		{
			Type_Native,
			Type_C,
		}; 
		Type type;

		string file;
		string toString() override { return string("Import(file:") + file + ")"; }	
	};

	struct StringLiteral : public NodeImpl<StringLiteral, Expression>
	{
		string value;
		string toString() override
		{
			string s = "StringLiteral(" + value + ")";
			return s;
		}

		Type getType() override
		{
			Type t;
			t.isString = true;
			return t;
		}
	};

	struct IntegerLiteral : public NodeImpl<IntegerLiteral, Expression>
	{
		string value;
		string toString() override 
		{ 
			string s = "IntegerLiteral(" + value + ")";
			return s;
		}

		Type getType() override
		{
			Type t;
			t.isInt = true;
			return t;
		}
	};

	struct FloatLiteral : public NodeImpl<FloatLiteral, Expression>
	{
		string value;
		string toString() override 
		{ 
			string s = "FloatLiteral(" + value + ")";
			return s;
		}

		Type getType() override
		{
			Type t;
			t.isFloat = true;
			return t;
		}
	};

	struct SymbolExpression : public NodeImpl<SymbolExpression, Expression>
	{
		string symbol;
		Symbol* symbolObj = nullptr;

		string toString() override 
		{ 
			string s = "SymbolExpression(" + symbol + ")";
			return s; 
		}

		Type getType() override
		{
			assert(symbolObj);
			assert(symbolObj->knowsType());
			return symbolObj->type;
		}
	};

	// TODO: split functions into calls and call-expression?
	//	for funcs that can act as an expression/operand
	struct Call : public NodeImpl<Call, Statement>
	{
		string function; // TODO: Remove?
		SymbolExpression* expr = nullptr;
		vector<Expression*> args;

		string toString() override 
		{ 
			string s = "Call(" + function + ")";
			return s; 
		}
		const vector<Node*> getChildren() override
		{
			vector<Node*> ret;
			assert(expr);
			ret.push_back(expr);
			ret.insert(ret.end(), args.begin(), args.end());
			return ret;
		}
	};

	struct UnaryOp : public NodeImpl<UnaryOp, Expression>
	{
		TokenType type;
		Expression* expr = nullptr;

		string toString() override 
		{ 
			string s = "UnaryOp(" + ::toString(type) + ")";
			return s; 
		}
		const vector<Node*> getChildren() override
		{
			vector<Node*> ret;
			ret.reserve(1);
			ret.push_back(expr);
			return ret;
		}

		Type getType() override
		{
			// TODO: Bubble up types through ops for now
			return expr->getType();
		}
	};

	struct UnaryPostfixOp : public NodeImpl<UnaryPostfixOp, Expression>
	{
		TokenType type;
		Expression* expr = nullptr;

		string toString() override 
		{ 
			string s = "UnaryPostfixOp(" + ::toString(type) + ")";
			return s; 
		}
		const vector<Node*> getChildren() override
		{
			vector<Node*> ret;
			ret.reserve(1);
			ret.push_back(expr);
			return ret;
		}

		Type getType() override
		{
			// TODO: Bubble up types through ops for now
			return expr->getType();
		}
	};

	struct BinaryOp : public NodeImpl<BinaryOp, Expression>
	{
		TokenType type;
		Expression* left = nullptr;
		Expression* right = nullptr;

		string toString() override 
		{ 
			string s = "BinaryOp(" + ::toString(type) + ")";
			return s; 
		}
		const vector<Node*> getChildren() override
		{
			vector<Node*> ret;
			ret.reserve(2);
			ret.push_back(left);
			ret.push_back(right);
			return ret;
		}

		Type getType() override
		{
			// TODO: Bubble up types through ops for now
			Type t = left->getType();
			assert(t == right->getType());
			return t;
		}
	};

	struct SymbolDeclaration : public NodeImpl<SymbolDeclaration, Declaration>
	{
		string symbol;
		bool isParam = false;
		SymbolExpression* typeExpr = nullptr;
		Expression* initExpr = nullptr;
		Symbol* symbolObj = nullptr;

		string toString() override 
		{ 
			string s = "SymbolDeclaration(" + symbol + ")";
			return s; 
		}

		const vector<Node*> getChildren() override
		{
			vector<Node*> ret;
			ret.reserve(2);
			if (typeExpr != nullptr)
				ret.push_back(typeExpr);
			if (initExpr != nullptr)
				ret.push_back(initExpr);
			return ret;
		}		
	};

	struct FuncLiteralSignature : public NodeImpl<FuncLiteralSignature>
	{
		vector<SymbolDeclaration*> params;

		string toString() override
		{
			string s = "FuncLiteralSignature";
			return s;
		}

		void addParameterDeclaration(SymbolDeclaration* declNode)
		{
			params.push_back(declNode);
		}

		const vector<Node*> getChildren() override
		{
			return vector<Node*>(params.begin(), params.end());
		}	
	};

	void visitChildren(FuncLiteralSignature* node, Visitor* v) { for (auto n : node->getChildren()) n->accept(v); }

	struct FunctionLiteral : public NodeImpl<FunctionLiteral, Expression>
	{
		FuncLiteralSignature* signature = nullptr;
		StatementBody* body = nullptr;

		string toString() override 
		{ 
			string s = "FunctionLiteral";
			return s;
		}

		Type getType() override
		{
			Type t;
			t.isFunction = true;
			return t;
		}

		const vector<Node*> getChildren() override
		{
			assert(signature);
			assert(body);
			auto ret =  vector<Node*>();
			ret.push_back(signature);
			ret.push_back(body);
			return ret;
		}
	};

	struct FunctionDeclaration : public NodeImpl<FunctionDeclaration, Declaration>
	{
		string symbol;
		FunctionLiteral* funcLiteral = nullptr;
		Symbol* symbolObj = nullptr;

		string toString() override 
		{ 
			string s;
			if (!funcLiteral)
				s = "External ";
			s += "FunctionDeclaration(" + symbol + ")";
			return s;
		}

		const vector<Node*> getChildren() override
		{
			auto ret =  vector<Node*>();
			if (funcLiteral)
				ret.push_back(funcLiteral);
			return ret;
		}
	};

	struct AST
	{
		Node* root = nullptr;
	};
}

vector<AST::Node*> s_nodes;

template<typename NodeT, typename... Args>
NodeT* createNode(Args... args)
{
	NodeT* n = new NodeT(std::forward<Args>(args)...);
	s_nodes.push_back(n);
	return n;
}
