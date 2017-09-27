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
	struct SymbolDeclaration;
	struct SymbolExpression;
	struct StringLiteral;
	struct IntegerLiteral;
	struct FloatLiteral;
	struct UnaryOp;
	struct UnaryPostfixOp;
	struct BinaryOp;

	struct Visitor;
	void visitChildren(Declaration* node, Visitor* v);
	void visitChildren(Statement* node, Visitor* v);
	void visitChildren(Module* node, Visitor* v);
	void visitChildren(Expression* node, Visitor* v);

	struct Visitor
	{
		virtual void visit(Declaration* node) { visitChildren(node, this); }
		virtual void visit(Statement* node) { visitChildren(node, this); }
		virtual void visit(Module* node) { visitChildren(node, this); }
		virtual void visit(Import* node) { visit((Statement*)node); }
		virtual void visit(Call* node) { visit((Statement*)node); }
		virtual void visit(SymbolDeclaration* node) { visit((Expression*)node);}	
		virtual void visit(Expression* node) { visitChildren(node, this); }
		virtual void visit(SymbolExpression* node) { visit((Expression*)node);}
		virtual void visit(StringLiteral* node) { visit((Expression*)node);}
		virtual void visit(IntegerLiteral* node) { visit((Expression*)node); }
		virtual void visit(FloatLiteral* node) { visit((Expression*)node); }
		virtual void visit(UnaryOp* node) { visit((Expression*)node); }
		virtual void visit(UnaryPostfixOp* node) { visit((Expression*)node); }
		virtual void visit(BinaryOp* node) { visit((Expression*)node); }
	};

	struct Node
	{
		virtual ~Node() = default;

		virtual const vector<Node*> getChildren() { return vector<Node*>(); }
		virtual void accept(Visitor* v) = 0;
		virtual string toString() = 0;
	};

	void visitChildren(Node* node, Visitor* v) { for (auto n : node->getChildren()) n->accept(v); }

	struct Statement : Node
	{};

	void visitChildren(Statement* node, Visitor* v) { for (auto n : node->getChildren()) n->accept(v); }

	struct Declaration : Statement
	{};

	void visitChildren(Declaration* node, Visitor* v) { for (auto n : node->getChildren()) n->accept(v); }

	struct Expression : Node
	{};

	void visitChildren(Expression* node, Visitor* v) { for (auto n : node->getChildren()) n->accept(v); }

	struct Scope : Node
	{
		SymbolScope scope;
		vector<Statement*> statements;

		string toString() override { return "Scope"; }
		const vector<Node*> getChildren() override
		{
			return vector<Node*>(statements.begin(), statements.end());
		}

		void addStatement(Statement* s)
		{
			statements.push_back(s);
		}	
	};

	template<typename T, typename P = Node>
	struct NodeImpl : P
	{
		void accept(Visitor* v) override { v->visit((T*)this); }
	};

	struct Module : public NodeImpl<Module, Scope>
	{

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
	};

	struct IntegerLiteral : public NodeImpl<IntegerLiteral, Expression>
	{
		string value;
		string toString() override 
		{ 
			string s = "IntegerLiteral(" + value + ")";
			return s;
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
	};

	// TODO: split functions into calls and call-expression?
	//	for funcs that can act as an expression/operand
	struct Call : public NodeImpl<Call, Statement>
	{
		string function;
		vector<Expression*> args;
		string toString() override 
		{ 
			string s = "Call(" + function + ")";
			return s; 
		}
		const vector<Node*> getChildren() override
		{
			return vector<Node*>(args.begin(), args.end());
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
	};

	struct SymbolDeclaration : public NodeImpl<SymbolDeclaration, Declaration>
	{
		string symbol;
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
			ret.push_back(typeExpr);
			ret.push_back(initExpr);
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
