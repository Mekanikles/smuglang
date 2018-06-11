#pragma once

struct FunctionArgumentBinding;

namespace AST
{
	struct Node;
	struct Statement;
	struct Declaration;
	struct Expression;
	struct Module;
	struct Import;
	struct Call;
	struct Assignment;
	struct IfStatement;
	struct SymbolDeclaration;
	struct FunctionDeclaration;
	struct SymbolExpression;
	struct Tuple;
	struct StringLiteral;
	struct IntegerLiteral;
	struct FloatLiteral;
	struct TypeLiteral;
	struct FunctionInParam;
	struct FunctionOutParam;
	struct FunctionSignature;
	struct FunctionLiteral;
	struct UnaryOp;
	struct UnaryPostfixOp;
	struct BinaryOp;
	struct EvalStatement;

	//struct FuncLiteralSignature;
	struct StatementBody;

	struct Visitor;
	void visitChildren(Node* node, Visitor* v);
	void visitChildren(Declaration* node, Visitor* v);
	void visitChildren(Statement* node, Visitor* v);
	void visitChildren(Module* node, Visitor* v);
	void visitChildren(Expression* node, Visitor* v);

	void visitChildren(StatementBody* node, Visitor* v);
	//void visitChildren(FuncLiteralSignature* node, Visitor* v);

	struct Visitor
	{
		// TODO: This sucks, every visit has an overahead of 3 stack frames going through
		//	the abstractions
		virtual void visit(Node* node) { visitChildren(node, this); }
		virtual void visit(Declaration* node) { visit((Node*)node); }
		virtual void visit(Statement* node) { visit((Node*)node); }
		virtual void visit(Module* node) { visit((Node*)node); }
		virtual void visit(Import* node) { visit((Statement*)node); }
		virtual void visit(Call* node) { visit((Statement*)node); }
		virtual void visit(Assignment* node) { visit((Statement*)node); }
		virtual void visit(IfStatement* node) { visit((Statement*)node); }
		virtual void visit(SymbolDeclaration* node) { visit((Statement*)node);}	
		virtual void visit(FunctionDeclaration* node) { visit((Statement*)node);}	
		virtual void visit(Expression* node) { visit((Node*)node); }
		virtual void visit(SymbolExpression* node) { visit((Expression*)node);}
		virtual void visit(StringLiteral* node) { visit((Expression*)node);}
		virtual void visit(IntegerLiteral* node) { visit((Expression*)node); }
		virtual void visit(FloatLiteral* node) { visit((Expression*)node); }
		virtual void visit(TypeLiteral* node) { visit((Expression*)node); }
		virtual void visit(FunctionInParam* node) { visit((Node*)node); }
		virtual void visit(FunctionOutParam* node) { visit((Node*)node); }
		virtual void visit(FunctionSignature* node) { visit((Expression*)node); }
		virtual void visit(FunctionLiteral* node) { visit((Expression*)node); }
		virtual void visit(UnaryOp* node) { visit((Expression*)node); }
		virtual void visit(UnaryPostfixOp* node) { visit((Expression*)node); }
		virtual void visit(BinaryOp* node) { visit((Expression*)node); }
		virtual void visit(EvalStatement* node) { visit((Statement*)node); }

		virtual void visit(StatementBody* node) { visit((Statement*)node); }
		//virtual void visit(FuncLiteralSignature* node) { visit((Node*)node); }
	};

	static uint s_nodeCount = 0;

	struct Node
	{
		virtual ~Node() = default;

		virtual const vector<Node*> getChildren() { return vector<Node*>(); }
		virtual void accept(Visitor* v) = 0;
		virtual string toString() = 0;

		SymbolScope* scopeRef = nullptr;
		uint order = s_nodeCount++;

		// This is used to prevent infinite recursion when processing the AST tree
		// TODO: Build a processing graph instead of testing for this bool
		bool processed = false;
	};

	void visitChildren(Node* node, Visitor* v) 
	{ 
		auto children = node->getChildren();
		for (auto n : children) 
		{
			//print("Visiting child node: "); printLine(n->toString());
			n->accept(v); 
		}
	}

	struct Statement : Node
	{};

	void visitChildren(Statement* node, Visitor* v) { for (auto n : node->getChildren()) n->accept(v); }

	struct Declaration : Statement
	{
	};

	void visitChildren(Declaration* node, Visitor* v) { for (auto n : node->getChildren()) n->accept(v); }

	static string typeString(const Type& t)
	{
		return string(", \033[35;1mType: \033[0m\033[35m") + t.toString() + string("\033[0m");
	}

	struct Expression : Statement
	{
		virtual Type& getType() = 0;

		virtual bool isSymbolExpression() { return false; }
	};

	void visitChildren(Expression* node, Visitor* v) { for (auto n : node->getChildren()) n->accept(v); }

	template<typename T, typename P = Node>
	struct NodeImpl : P
	{
		void accept(Visitor* v) override 
		{ 
			//print("Accepting node: "); printLine(this->toString());
			v->visit((T*)this); 
		}
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

	struct EvalStatement : public NodeImpl<EvalStatement, StatementBody>
	{
		Expression* expr = nullptr;
		// TODO: Try to remove this, as it is only used during processing
		CatchAllSymbolSource* catchAllSource = nullptr;
		bool isGenerated = false;

		string toString() override
		{
			string s = "EvalStatement";
			return s;
		}

		const vector<Node*> getChildren() override
		{
			if (!this->isGenerated)
			{
				vector<Node*> ret;
				ret.push_back(expr);
				return ret;
			}

			return StatementBody::getChildren();
		}
	};

	struct StringLiteral : public NodeImpl<StringLiteral, Expression>
	{
		string value;
		Type type;
		
		StringLiteral(const string& value)
			: value(value)
			, type(createMultiTypeVariable(
					createStaticArrayType(createPrimitiveType(PrimitiveClass::Char), value.length()),
					createPointerType(createPrimitiveType(PrimitiveClass::Char))))
		{
		}

		string toString() override
		{
			string val = processQuotedInputString(value);
			string str = processStringForOutput(val);
			string s = "StringLiteral(\"" + str + "\")";
			s += typeString(getType());
			return s;
		}

		Type& getType() override
		{
			return type;
		}
	};

	struct IntegerLiteral : public NodeImpl<IntegerLiteral, Expression>
	{
		string value;
		Type type;

		IntegerLiteral(const string& value)
			: value(value)
			, type(createPrimitiveType(PrimitiveClass::Int))
		{
		}

		string toString() override 
		{ 
			string s = "IntegerLiteral(" + value + ")";
			s += typeString(getType());
			return s;
		}

		Type& getType() override
		{
			return type;
		}
	};

	struct FloatLiteral : public NodeImpl<FloatLiteral, Expression>
	{
		string value;
		Type type;
		
		FloatLiteral(const string& value)
			: value(value)
			, type(createPrimitiveType(PrimitiveClass::Float))
		{
		}

		string toString() override 
		{ 
			string s = "FloatLiteral(" + value + ")";
			s += string(", Type: ") + type.toString();
			return s;
		}

		Type& getType() override
		{
			return type;
		}
	};

	struct TypeLiteral : public NodeImpl<TypeLiteral, Expression>
	{
		Type type;
		TypeLiteral(std::unique_ptr<TypeClass> typeClass)
			: type(createTypeVariable(std::move(typeClass)))
		{}

		string toString() override 
		{ 
			string s = "TypeLiteral";
			s += typeString(getType());
			return s;
		}

		Type& getType() override
		{
			return type;
		}	
	};

	struct FunctionInParam : public NodeImpl<FunctionInParam, Node>
	{
		string name;
		Expression* typeExpr = nullptr;
		Expression* initExpr = nullptr;
		bool isVariadic = false;
		Type type;

		Type& getType()
		{
			if (typeExpr)
			{
				const auto result = unifyTypes(type, typeExpr->getType());
				assert(result != CannotUnify);
			}
			if (initExpr)
			{
				const auto result = unifyTypes(type, typeExpr->getType());
				if (result == CannotUnify)
					assert("Cannot unify types" && false);
			}
			return type;
		}

		string toString() override 
		{ 
			string s = "FunctionInParam(" + name + ")";
			s += typeString(getType());
			return s;
		}

		const vector<Node*> getChildren() override
		{
			auto ret =  vector<Node*>();
			if (typeExpr)
				ret.push_back(typeExpr);
			if (initExpr)
				ret.push_back(initExpr);
			return ret;
		}
	};

	struct FunctionOutParam : public NodeImpl<FunctionInParam, Node>
	{
		string name;
		Expression* typeExpr = nullptr;

		Type& getType()
		{
			assert(typeExpr);
			return typeExpr->getType();
		}

		string toString() override 
		{ 
			string s = "FunctionOutParam(" + name + ")";
			s += typeString(getType());
			return s;
		}

		const vector<Node*> getChildren() override
		{
			assert(typeExpr);
			auto ret =  vector<Node*>();
			if (typeExpr)
				ret.push_back(typeExpr);
			return ret;
		}		
	};

	struct FunctionSignature : public NodeImpl<FunctionSignature, Expression>
	{
		vector<FunctionInParam*> inParams;
		vector<FunctionOutParam*> outParams;
		bool specifiedInParams = false;
		bool specifiedOutParams = false;
		bool isCVariadic = false;
		Type type;

		string toString() override 
		{ 
			string s = "FunctionSignature(isCVariadic: " + std::to_string(isCVariadic) +
					", isAny: " + std::to_string(type.isAny()) + ")";
			s += typeString(getType());	
			return s;
		}

		Type& getType() override
		{
			// TODO:
			if (type.isAny())
			{
				auto typeClass = std::make_unique<FunctionClass>();
				typeClass->isCVariadic = isCVariadic;

				for (auto* p : inParams)
				{
					Type& t = p->getType();
					typeClass->inTypes.push_back(t);
				}

				for (auto* p : outParams)
				{
					Type& t = p->getType();
					typeClass->outTypes.push_back(t);
				}

				type = createTypeVariable(std::move(typeClass));
			}
			return type;
		}

		const vector<Node*> getChildren() override
		{
			vector<Node*> ret;
			ret.insert(ret.end(), inParams.begin(), inParams.end());
			ret.insert(ret.end(), outParams.begin(), outParams.end());
			return ret;
		}		
	};

	struct SymbolExpression : public NodeImpl<SymbolExpression, Expression>
	{
		string symbol;
		SymbolDependency* dependency = nullptr;

		SymbolExpression(string symbol)
			: symbol(symbol)
		{}	

		bool isSymbolExpression() override { return true; }

		Symbol* getSymbol()
		{
			assert(dependency);
			return dependency->getSymbol();
		}

		string toString() override 
		{ 
			string s;
			s = "SymbolExpression(" + symbol + ")";
			s += typeString(getType());
			return s; 
		}

		Type& getType() override
		{
			return getSymbol()->type;
		}
	};

	struct Tuple : public NodeImpl<Tuple, Expression>
	{
		vector<Expression*> exprs;
		Type type;

		string toString() override 
		{ 
			string s;
			s = "Tuple";
			s += typeString(getType());
			return s; 
		}

		Type& getType() override
		{
			// TODO:
			if (type.isAny())
			{
				vector<Type> types;
				for (auto* e : exprs)				
					types.push_back(e->getType());

				type = createTupleType(std::move(types));
			}
			return type;
		}
	};

	// TODO: split functions into calls and call-expression?
	//	for funcs that can act as an expression/operand
	struct Call : public NodeImpl<Call, Statement>
	{
		string function; // TODO: Remove?
		SymbolExpression* expr = nullptr; // TODO: Can be any expression
		vector<Expression*> args;

		FunctionArgumentBinding* argBinding = nullptr;

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

	struct Assignment : public NodeImpl<Assignment, Statement>
	{
		SymbolExpression* symExpr;
		Expression* expr = nullptr;

		string toString() override 
		{ 
			assert(symExpr);
			string s = "Assigment";
			return s; 
		}

		const vector<Node*> getChildren() override
		{
			vector<Node*> ret;
			ret.reserve(2);
			ret.push_back(symExpr);
			ret.push_back(expr);
			return ret;
		}	
	};

	struct UnaryOp : public NodeImpl<UnaryOp, Expression>
	{
		TokenType opType;
		Expression* expr = nullptr;

		string toString() override 
		{ 
			string s = "UnaryOp(" + ::toString(opType) + ")";
			s += typeString(getType());
			return s; 
		}
		const vector<Node*> getChildren() override
		{
			vector<Node*> ret;
			ret.reserve(1);
			ret.push_back(expr);
			return ret;
		}

		Type& getType() override
		{
			// TODO: Bubble up types through ops for now
			Type& t = expr->getType();
			return t;
		}
	};

	struct UnaryPostfixOp : public NodeImpl<UnaryPostfixOp, Expression>
	{
		TokenType opType;
		Expression* expr = nullptr;
		Type type;

		string toString() override 
		{ 
			string s = "UnaryPostfixOp(" + ::toString(opType) + ")";
			s += typeString(getType());
			return s; 
		}
		const vector<Node*> getChildren() override
		{
			vector<Node*> ret;
			ret.reserve(1);
			ret.push_back(expr);
			return ret;
		}

		Type& getType() override
		{
			if (type.isAny())
			{
				// TODO: Bubble up types through ops for now
				const Type& t = expr->getType();
				if (opType == TokenType::Asterisk)
				{
					type = createPointerTypeVariable(t.innerTypeFromTypeVariable());
				}
				else
				{
					type = t;
				}
			}
			return type;
		}
	};

	struct BinaryOp : public NodeImpl<BinaryOp, Expression>
	{
		TokenType opType;
		Expression* left = nullptr;
		Expression* right = nullptr;

		string toString() override 
		{ 
			string s = "BinaryOp(" + ::toString(opType) + ")";
			s += typeString(getType());
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

		Type& getType() override
		{
			// TODO: Bubble up types through ops for now
			Type& t1 = left->getType();
			Type& t2 = right->getType();

			const auto result = unifyTypes(t1, t2);
			if (result == CannotUnify)
				assert("Cannot unify types" && false);

			return t1;
		}
	};

	struct SymbolDeclaration : public NodeImpl<SymbolDeclaration, Declaration>
	{
		string symbol;
		bool isParam = false;
		bool isExternal = false;
		Expression* typeExpr = nullptr;
		Expression* initExpr = nullptr;
		Symbol* symbolObj = nullptr;

		string toString() override 
		{ 
			string s = "SymbolDeclaration(" + symbol + ", isExternal = " + 
					std::to_string(isExternal) + ", isParam: " + 
					std::to_string(isParam) + ")";
			return s; 
		}

		Symbol* getSymbol()
		{
			return symbolObj;
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

	/*struct FuncLiteralSignature : public NodeImpl<FuncLiteralSignature>
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
	};*/

	//void visitChildren(FuncLiteralSignature* node, Visitor* v) { for (auto n : node->getChildren()) n->accept(v); }

	struct FunctionLiteral : public NodeImpl<FunctionLiteral, Expression>
	{
		Type type;
		//FuncLiteralSignature* signature = nullptr;
		FunctionSignature* signature = nullptr;
		StatementBody* body = nullptr;

		FunctionLiteral()
			: type(createFunctionType())
		{}	

		string toString() override 
		{ 
			string s = "FunctionLiteral";
			s += typeString(getType());
			return s;
		}

		Type& getType() override
		{
			return type;
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

	// TODO: Replace with normal symbol declaration/definition
	struct FunctionDeclaration : public NodeImpl<FunctionDeclaration, Declaration>
	{
		string symbol;
		FunctionLiteral* funcLiteral = nullptr;
		SymbolSource* symbolSource = nullptr;

		string toString() override 
		{ 
			string s;
			s += "FunctionDeclaration(" + symbol + ")";
			return s;
		}

		Symbol* getSymbol()
		{
			assert(symbolSource);
			return symbolSource->getSymbol();
		}

		const vector<Node*> getChildren() override
		{
			auto ret =  vector<Node*>();
			if (funcLiteral)
				ret.push_back(funcLiteral);
			return ret;
		}
	};

	struct ASTObject
	{
		Node* root = nullptr;
	};
}

template<typename NodeT, typename... Args>
NodeT* createNode(Args... args)
{
	NodeT* n = createObject<NodeT>(std::forward<Args>(args)...);
	return n;
}
