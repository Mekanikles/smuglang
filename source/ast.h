#pragma once

struct ArgumentBinding;

namespace IR
{
	struct Literal;
}

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
	struct TemplateDeclaration;
	struct SymbolDeclaration;
	struct FunctionDeclaration;
	struct StructField;
	struct StructDeclaration;
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
	struct DeferStatement;
	struct ReturnStatement;
	struct MemberAccess;
	struct LoopStatement;
	struct ContinueStatement;
	struct BreakStatement;

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
		virtual void visit(Declaration* node) { visit((Statement*)node); }
		virtual void visit(Statement* node) { visit((Node*)node); }
		virtual void visit(Module* node) { visit((Node*)node); }
		virtual void visit(Import* node) { visit((Statement*)node); }
		virtual void visit(Call* node) { visit((Statement*)node); }
		virtual void visit(Assignment* node) { visit((Statement*)node); }
		virtual void visit(IfStatement* node) { visit((Statement*)node); }
		virtual void visit(LoopStatement* node) { visit((Statement*)node); }
		virtual void visit(ContinueStatement* node) { visit((Statement*)node); }
		virtual void visit(BreakStatement* node) { visit((Statement*)node); }
		virtual void visit(TemplateDeclaration* node) { visit((Statement*)node); }
		virtual void visit(SymbolDeclaration* node) { visit((Declaration*)node);}	
		virtual void visit(FunctionDeclaration* node) { visit((Declaration*)node);}
		virtual void visit(StructField* node) { visit((Node*)node);}
		virtual void visit(StructDeclaration* node) { visit((Declaration*)node);}
		virtual void visit(Expression* node) { visit((Node*)node); }
		virtual void visit(SymbolExpression* node) { visit((Expression*)node);}
		virtual void visit(MemberAccess* node) { visit((Expression*)node);}
		virtual void visit(StringLiteral* node) { visit((Expression*)node);}
		virtual void visit(IntegerLiteral* node) { visit((Expression*)node); }
		virtual void visit(FloatLiteral* node) { visit((Expression*)node); }
		virtual void visit(TypeLiteral* node) { visit((Expression*)node); }
		virtual void visit(Tuple* node) { visit((Expression*)node); }
		virtual void visit(FunctionInParam* node) { visit((Node*)node); }
		virtual void visit(FunctionOutParam* node) { visit((Node*)node); }
		virtual void visit(FunctionSignature* node) { visit((Expression*)node); }
		virtual void visit(FunctionLiteral* node) { visit((Expression*)node); }
		virtual void visit(UnaryOp* node) { visit((Expression*)node); }
		virtual void visit(UnaryPostfixOp* node) { visit((Expression*)node); }
		virtual void visit(BinaryOp* node) { visit((Expression*)node); }
		virtual void visit(EvalStatement* node) { visit((StatementBody*)node); }
		virtual void visit(DeferStatement* node) { visit((Statement*)node); }
		virtual void visit(ReturnStatement* node) { visit((Statement*)node); }

		virtual void visit(StatementBody* node) { visit((Statement*)node); }
		//virtual void visit(FuncLiteralSignature* node) { visit((Node*)node); }
	};

	static uint s_nodeCount = 0;

	struct Node
	{
		virtual ~Node() = default;

		virtual const vector<Node*> getChildren() { return vector<Node*>(); }
		virtual void accept(Visitor* v) = 0;
		virtual string toString(ASTContext* context) = 0;

		uint order = s_nodeCount++;
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
		virtual string getSymbolName() = 0;
	};

	void visitChildren(Declaration* node, Visitor* v) { for (auto n : node->getChildren()) n->accept(v); }

	static string symbolString(const string& s)
	{
		return string("\033[94;1m") + s + string("\033[0m");
	}

	struct Expression : Statement
	{
		virtual TypeRef& getType(ASTContext* context) = 0;

		virtual bool isSymbolExpression() { return false; }
		virtual bool isMemberAccess() { return false; }
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
		vector<Statement*> statements;

		string toString(ASTContext* context) override { return "StatementBody"; }
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

		string toString(ASTContext* context) override { return "Module"; }
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

		string toString(ASTContext* context) override 
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
		enum LinkType
		{
			LinkType_Native,
			LinkType_C,
		};

		LinkType linkType;

		string file;
		string toString(ASTContext* context) override { return string("Import(file:") + file + ")"; }	
	};

	struct LoopStatement : public NodeImpl<LoopStatement, Statement>
	{
		Statement* statement = nullptr;

		string toString(ASTContext* context) override 
		{ 
			string s = "LoopStatement";
			return s; 
		}

		const vector<Node*> getChildren() override
		{
			vector<Node*> ret;
			ret.push_back(statement);

			return ret;
		}
	};

	struct ContinueStatement : public NodeImpl<ContinueStatement, Statement>
	{
		string toString(ASTContext* context) override 
		{ 
			string s = "ContinueStatement";
			return s; 
		}
	};

	struct BreakStatement : public NodeImpl<BreakStatement, Statement>
	{
		string toString(ASTContext* context) override 
		{ 
			string s = "BreakStatement";
			return s; 
		}
	};

	struct EvalStatement : public NodeImpl<EvalStatement, StatementBody>
	{
		Expression* expr = nullptr;
		bool isGenerated = false;

		string toString(ASTContext* context) override
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

			// Wtf?
			return StatementBody::getChildren();
		}
	};

	struct DeferStatement : public NodeImpl<DeferStatement, Statement>
	{
		Statement* statement;

		string toString(ASTContext* context) override
		{
			string s = "DeferStatement";
			return s;
		}

		const vector<Node*> getChildren() override
		{
			vector<Node*> ret;
			if (statement)
				ret.push_back(statement);
			return ret;
		}
	};

	struct ReturnStatement : public NodeImpl<ReturnStatement, Statement>
	{
		Expression* expr = nullptr;

		string toString(ASTContext* context) override
		{
			string s = "ReturnStatement";
			return s;
		}

		const vector<Node*> getChildren() override
		{
			vector<Node*> ret;
			if (expr)
				ret.push_back(expr);
			return ret;
		}
	};

	struct StringLiteral : public NodeImpl<StringLiteral, Expression>
	{
		string value;
		
		StringLiteral(const string& value)
			: value(value)
		{
		}

		TypeRef createLiteralType()
		{
			TypeRef type = TypeRef(createMultiTypeVariable());
			auto& multiType = type->getMultiType();
			multiType.appendType(createStaticArrayType(createPrimitiveType(PrimitiveClass::Char), value.length()));
			multiType.appendType(createPointerType(createPrimitiveType(PrimitiveClass::Char)));
			return type;
		}

		string toString(ASTContext* context) override
		{
			string val = processQuotedInputString(value);
			string str = processStringForOutput(val);
			string s = "StringLiteral(\"" + str + "\")";
			s += typeString(getType(context));
			return s;
		}

		TypeRef& getType(ASTContext* context) override
		{
			return context->getTypeLiteral(this);
		}
	};

	struct IntegerLiteral : public NodeImpl<IntegerLiteral, Expression>
	{
		enum LiteralType
		{
			Decimal,
			Hexadecimal,
			Binary
		};

		string value;
		LiteralType ltype;

		IntegerLiteral(const string& value, LiteralType ltype = Decimal)
			: value(value)
			, ltype(ltype)
		{
		}

		TypeRef createLiteralType()
		{
			// TODO: Check for size etc, so that we filter out types that won't fit the value
			TypeRef type = TypeRef(createMultiTypeVariable());
			auto& multiType = type->getMultiType();
			multiType.appendType(createPrimitiveType(PrimitiveClass::Int));
			// Ints can also be floats
			// TODO: Replace IntegerLiteral with DecimalLiteral (or something)
			multiType.appendType(createPrimitiveType(PrimitiveClass::Float));
			return type;
		}

		string toString(ASTContext* context) override 
		{ 
			string s = "IntegerLiteral(" + value + ")";
			s += typeString(getType(context));
			return s;
		}

		TypeRef& getType(ASTContext* context) override
		{
			return context->getTypeLiteral(this);
		}
	};

	struct FloatLiteral : public NodeImpl<FloatLiteral, Expression>
	{
		string value;

		FloatLiteral(const string& value)
			: value(value)
		{
		}

		TypeRef createLiteralType()
		{
			return createPrimitiveType(PrimitiveClass::Float);
		}		

		string toString(ASTContext* context) override 
		{ 
			string s = "FloatLiteral(" + value + ")";
			s += typeString(getType(context));
			return s;
		}

		TypeRef& getType(ASTContext* context) override
		{
			return context->getTypeLiteral(this);
		}
	};

	struct TypeLiteral : public NodeImpl<TypeLiteral, Expression>
	{
		TypeRef type;
		TypeLiteral(TypeRef&& type)
			: type(std::move(type))
		{}

		// TODO: It only makes sense to call this once
		//	probably move out all type creation to some processor
		TypeRef createLiteralType()
		{
			// Hm, this is called mulitple times, possibly in multiple
			//	contexts, make sure not to share this type across different inferences
			return createTypeVariable(type.clone());
		}

		string toString(ASTContext* context) override 
		{ 
			string s = "TypeLiteral";
			s += typeString(getType(context));
			return s;
		}

		TypeRef& getType(ASTContext* context) override
		{
			return context->getTypeLiteral(this);
		}
	};

	struct FunctionInParam : public NodeImpl<FunctionInParam, Node>
	{
		string name;
		Expression* typeExpr = nullptr;
		Expression* initExpr = nullptr;
		StorageQualifier storageQualifier = StorageQualifier::Const;
		bool isVariadic = false;

		Symbol* getSymbol(ASTContext* context)
		{
			auto* symbolSource = context->getSymbolSource(this);
			assert(symbolSource);
			return symbolSource->getSymbol();
		}

		TypeRef& getType(ASTContext* context)
		{
			return getSymbol(context)->getType();
		}

		const string& getName(ASTContext* context)
		{
			return getSymbol(context)->name;
		}

		string toString(ASTContext* context) override 
		{ 	
			string s = "FunctionInParam(" + symbolString(name) + ")";
			s += typeString(getType(context));
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

	struct FunctionOutParam : public NodeImpl<FunctionOutParam, Node>
	{
		string name;
		Expression* typeExpr = nullptr;
		StorageQualifier storageQualifier = StorageQualifier::Var;

		Symbol* getSymbol(ASTContext* context)
		{
			auto* symbolSource = context->getSymbolSource(this);
			assert(symbolSource);
			return symbolSource->getSymbol();
		}

		TypeRef& getType(ASTContext* context)
		{
			return getSymbol(context)->getType();
		}

		const string& getName(ASTContext* context)
		{
			return getSymbol(context)->name;
		}

		string toString(ASTContext* context) override 
		{ 
			string s = "FunctionOutParam(" + symbolString(name) + ")";
			s += typeString(getType(context));
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

		string toString(ASTContext* context) override 
		{ 
			string s = "FunctionSignature(isAny: " + std::to_string(getType(context).getType().isAny()) + ")";
			s += typeString(getType(context));	
			return s;
		}

		TypeRef createLiteralType(ASTContext* context)
		{
			auto func = std::make_unique<FunctionClass>();
			
			for (auto* p : inParams)
			{
				TypeRef& t = p->getType(context);
				func->appendInParam(TypeRef(t), p->getName(context));
			}

			for (auto* p : outParams)
			{
				TypeRef& t = p->getType(context);
				func->appendOutParam(TypeRef(t), p->getName(context));
			}

			return createTypeVariable(Type(std::move(func)));
		}

		TypeRef& getType(ASTContext* context) override
		{
			return context->getTypeLiteral(this);
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
		vector<Expression*> templateArgs;

		SymbolExpression(string symbol)
			: symbol(symbol)
		{}	

		bool isSymbolExpression() override { return true; }

		bool hasSymbol(ASTContext* context)
		{
			return context->getSymbolDependency(this) != nullptr;
		}

		pair<Node*, ASTContext*> getNodeForDependency(ASTContext* context)
		{
			auto* symbolDependency = context->getSymbolDependency(this);
			assert(symbolDependency);
			auto* symbolSource = symbolDependency->getHookedSymbolSource();
			assert(symbolSource);
			return symbolSource->getNode();
		}

		Symbol* getSymbol(ASTContext* context)
		{
			auto* symbolDependency = context->getSymbolDependency(this);
			assert(symbolDependency);
			return symbolDependency->getSymbol();
		}

		string toString(ASTContext* context) override 
		{ 
			string s;
			s = "SymbolExpression(" + symbol + ")";
			if (hasSymbol(context))
				s += typeString(getType(context));
			return s; 
		}

		TypeRef& getType(ASTContext* context) override
		{
			return getSymbol(context)->getType();
		}

		const vector<Node*> getChildren() override
		{
			vector<Node*> ret;
			ret.insert(ret.end(), templateArgs.begin(), templateArgs.end());
			return ret;
		}	
	};

	struct Tuple : public NodeImpl<Tuple, Expression>
	{
		vector<Expression*> exprs;

		string toString(ASTContext* context) override 
		{ 
			string s;
			s = "Tuple";
			s += typeString(getType(context));
			return s; 
		}

		TypeRef& getType(ASTContext* context) override
		{
			return context->getTypeLiteral(this);
		}

		const vector<Node*> getChildren() override
		{
			vector<Node*> ret;
			ret.insert(ret.end(), exprs.begin(), exprs.end());
			return ret;
		}	
	};

	// TODO: split functions into calls and call-expression?
	//	for funcs that can act as an expression/operand
	struct Call : public NodeImpl<Call, Expression>
	{
		Expression* expr = nullptr;
		vector<Expression*> args;

		ArgumentBinding* argBinding = nullptr;

		string toString(ASTContext* context) override 
		{ 
			string s = "Call";
			if (hasSymbol(context))
				s += typeString(getType(context));
			return s; 
		}

		bool hasSymbol(ASTContext* context)
		{
			return context->getSymbolDependency(this->expr) != nullptr;
		}

		Symbol* getSymbol(ASTContext* context)
		{
			auto* symbolDependency = context->getSymbolDependency(this->expr);
			assert(symbolDependency);
			return symbolDependency->getSymbol();
		}

		TypeRef& getType(ASTContext* context) override
		{
			auto& type = getSymbol(context)->getType();
			FunctionClass& function = type->getFunction();
			if (function.outParams.empty())
			{
				return s_voidType;
			}
			else
			{
				// TODO: Multiple params :(
				return function.outParams.back().type;
			}
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
		Expression* target;
		Expression* expr = nullptr;

		string toString(ASTContext* context) override 
		{ 
			string s = "Assigment";
			return s; 
		}

		const vector<Node*> getChildren() override
		{
			vector<Node*> ret;
			ret.reserve(2);
			ret.push_back(target);
			ret.push_back(expr);
			return ret;
		}	
	};

	struct UnaryOp : public NodeImpl<UnaryOp, Expression>
	{
		TokenType opType;
		Expression* expr = nullptr;

		string toString(ASTContext* context) override 
		{ 
			string s = "UnaryOp(" + ::toString(opType) + ")";
			s += typeString(getType(context));
			return s; 
		}

		const vector<Node*> getChildren() override
		{
			vector<Node*> ret;
			ret.reserve(1);
			ret.push_back(expr);
			return ret;
		}

		TypeRef& getType(ASTContext* context) override
		{
			// TODO: Bubble up types through ops for now
			return expr->getType(context);
		}
	};

	struct UnaryPostfixOp : public NodeImpl<UnaryPostfixOp, Expression>
	{
		TokenType opType;
		Expression* expr = nullptr;

		string toString(ASTContext* context) override 
		{ 
			string s = "UnaryPostfixOp(" + ::toString(opType) + ")";
			s += typeString(getType(context));
			return s; 
		}

		const vector<Node*> getChildren() override
		{
			vector<Node*> ret;
			ret.reserve(1);
			ret.push_back(expr);
			return ret;
		}

		TypeRef& getType(ASTContext* context) override
		{
			return context->getTypeLiteral(this);
		}		
	};

	struct BinaryOp : public NodeImpl<BinaryOp, Expression>
	{
		TokenType opType;
		Expression* left = nullptr;
		Expression* right = nullptr;
		std::optional<TypeRef> type;

		string toString(ASTContext* context) override 
		{ 
			string s = "BinaryOp(" + ::toString(opType) + ")";
			s += typeString(getType(context));
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

		TypeRef& getType(ASTContext* context) override
		{
			// TODO: Bubble up types through ops for now
			TypeRef& t1 = left->getType(context);
			TypeRef& t2 = right->getType(context);

			assert(t1 == t2 && "Binary op types should have been unified");

			return t1;
		}
	};

	// NOTE: We don't want to be able to nest template declarations, so inherit Statement instead of declaration
	struct TemplateDeclaration : public NodeImpl<TemplateDeclaration, Statement>
	{
		FunctionSignature* signature = nullptr;	
		Declaration* declaration = nullptr;

		struct Instance
		{
			struct LiteralAndSource
			{
				shared<IR::Literal> literal;
				SymbolSource* source;
				string name;
			};

			Instance(ASTContext&& astContext, vector<Instance::LiteralAndSource>&& literals, string debugName)
				: astContext(std::move(astContext))
				, literals(std::move(literals))
			{}
			
			ASTContext astContext;
			vector<LiteralAndSource> literals;			
		};

		vector<Instance> instances;

		string toString(ASTContext* context) override 
		{
			string s;
			if (instances.size() > 0)
				s = "Template with " + std::to_string(instances.size()) + " instances";
			else
				s = "Unused Template ";
			return s; 
		}

		Instance& addInstance(ASTContext&& astContext, vector<Instance::LiteralAndSource>&& literals)
		{
			string debugName = string("template ") + declaration->getSymbolName() + 
				string(", instance: ") + std::to_string(instances.size());
			instances.push_back(Instance(std::move(astContext), std::move(literals), debugName));
			return instances.back();
		}
	};

	struct SymbolDeclaration : public NodeImpl<SymbolDeclaration, Declaration>
	{
		string symbol;
		StorageQualifier storageQualifier = StorageQualifier::Var;
		Expression* typeExpr = nullptr;
		Expression* initExpr = nullptr;
		bool isStatic = false;

		virtual string getSymbolName() override
		{
			return symbol;
		}

		string toString(ASTContext* context) override 
		{ 
			string s = "SymbolDeclaration(" + symbolString(symbol) + ", SQ = " + 
					sqToString(storageQualifier) + ")" + typeString(getType(context));
			return s; 
		}

		bool isExternal() { return storageQualifier == StorageQualifier::Extern; }
		bool isConst() { return storageQualifier == StorageQualifier::Const; }
		bool isDefine() { return storageQualifier == StorageQualifier::Def; }

		Symbol* getSymbol(ASTContext* context)
		{
			auto* symbolSource = context->getSymbolSource(this);
			return symbolSource->getSymbol();
		}

		const TypeRef& getType(ASTContext* context)
		{
			return getSymbol(context)->getType();
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

	struct MemberAccess : public NodeImpl<MemberAccess, Expression>
	{
		Expression* expr;
		SymbolExpression* member;
		TypeRef type;

		MemberAccess()
		{}

		virtual bool isMemberAccess() override
		{
			return true;
		}

		string getMemberName() const
		{
			return member->symbol;
		}

		string toString(ASTContext* context) override 
		{ 
			string s;
			s = "MemberAccess";
			return s; 
		}

		TypeRef& getType(ASTContext* context) override
		{
			return type;
		}

		const vector<Node*> getChildren() override
		{
			assert(expr);
			auto ret =  vector<Node*>();
			ret.push_back(expr);
			return ret;
		}		
	};

	struct FunctionLiteral : public NodeImpl<FunctionLiteral, Expression>
	{
		FunctionSignature* signature = nullptr;
		StatementBody* body = nullptr;

		FunctionLiteral()
		{}	

		string toString(ASTContext* context) override 
		{ 
			string s = "FunctionLiteral";
			s += typeString(getType(context));
			return s;
		}

		TypeRef& getType(ASTContext* context) override
		{
			assert(signature);
			auto& signType = signature->getType(context);
			auto& type = signType->getTypeVariable().type;
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

		virtual string getSymbolName() override
		{
			return symbol;
		}

		string toString(ASTContext* context) override 
		{ 
			string s;
			s += "FunctionDeclaration(" + symbolString(symbol) + ")";
			return s;
		}

		Symbol* getSymbol(ASTContext* context)
		{
			auto* symbolSource = context->getSymbolSource(this);
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

	struct StructField : public NodeImpl<StructField, Node>
	{
		string name;
		Expression* typeExpr = nullptr;
		Expression* initExpr = nullptr;

		Symbol* getSymbol(ASTContext* context)
		{
			auto* symbolSource = context->getSymbolSource(this);
			assert(symbolSource);
			return symbolSource->getSymbol();
		}

		TypeRef& getType(ASTContext* context)
		{
			return getSymbol(context)->getType();
		}

		const string& getName(ASTContext* context)
		{
			return getSymbol(context)->name;
		}

		string toString(ASTContext* context) override 
		{ 	
			string s = "StructField(" + symbolString(name) + ")";
			s += typeString(getType(context));
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

	// TODO: Replace with normal symbol declaration/definition
	struct StructDeclaration : public NodeImpl<StructDeclaration, Declaration>
	{
		string name;
		vector<StructField*> fields;

		TypeRef createLiteralType(ASTContext* context)
		{
			auto structType = std::make_unique<StructClass>(name);
			
			for (auto* f : fields)
			{
				TypeRef& t = f->getType(context);
				structType->addField(TypeRef(t), f->getName(context));
			}

			return createTypeVariable(Type(std::move(structType)));
		}

		virtual string getSymbolName() override
		{
			return name;
		}

		string toString(ASTContext* context) override 
		{ 
			string s;
			s += "StructDeclaration(" + symbolString(name) + ")";
			return s;
		}

		Symbol* getSymbol(ASTContext* context)
		{
			auto* symbolSource = context->getSymbolSource(this);
			assert(symbolSource);
			return symbolSource->getSymbol();
		}

		const vector<Node*> getChildren() override
		{
			vector<Node*> ret;
			ret.insert(ret.end(), fields.begin(), fields.end());
			return ret;
		}
	};

	struct ASTObject
	{
		Module* module = nullptr;
	};
}

template<typename NodeT, typename... Args>
NodeT* createNode(Args... args)
{
	NodeT* n = createObject<NodeT>(std::forward<Args>(args)...);
	return n;
}
