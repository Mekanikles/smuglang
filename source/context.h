#pragma once
#include <unordered_map>

namespace AST
{
	struct Node;
	struct Expression;
}

namespace IR
{
	struct Literal;
}

struct ASTContext
{
	vector<SymbolScope*> symbolScopes;

	struct NodeInfo
	{
		SymbolScope* scope = nullptr;

		// This is used to prevent infinite recursion when processing the AST tree
		// TODO: Build a processing graph instead of testing for this bool
		bool processed = false;

		// TODO: All nodes get these now
		SymbolSource* symbolSource = nullptr;
		SymbolDependency* symbolDependency = nullptr;

		// TODO: Only used during processing, store somewhere else
		CatchAllSymbolSource* catchAllSource = nullptr;
	};

	std::unordered_map<AST::Node*, NodeInfo> astNodeMap;
	std::unordered_map<AST::Node*, TypeRef> typeLiterals;

	struct ExpressionAndContext
	{
		AST::Expression* expr = nullptr;
		ASTContext* context = nullptr;
	};

	std::unordered_map<AST::Node*, ExpressionAndContext> templateExpressions;
	std::unordered_map<AST::Node*, shared<IR::Literal>> templateLiterals;

	bool processCheck(AST::Node* node)
	{
		NodeInfo& info = this->astNodeMap[node];
		if (!info.processed)
		{
			info.processed = true;
			return false;
		}
		return true;
	}

	void addTypeLiteral(AST::Node* node, TypeRef&& type)
	{
		this->typeLiterals[node] = std::move(type);
	}

	TypeRef& getTypeLiteral(AST::Node* node)
	{
		return this->typeLiterals[node];
	}

	void addTemplateExpression(AST::Node* node, AST::Expression* expr, ASTContext* context)
	{	
		this->templateExpressions[node] = ExpressionAndContext{ expr, context };
	}

	ExpressionAndContext& getTemplateExpression(AST::Node* node)
	{
		return this->templateExpressions[node];
	}

	void addTemplateLiteral(AST::Node* node, shared<IR::Literal> literal)
	{	
		this->templateLiterals[node] = literal;
	}

	shared<IR::Literal> getTemplateLiteral(AST::Node* node)
	{
		return this->templateLiterals[node];
	}

	SymbolScope* createScope(SymbolScope* parentScope = nullptr)
	{
		this->symbolScopes.push_back(new SymbolScope());
		SymbolScope* scope = this->symbolScopes.back();
		scope->parentScope = parentScope;
		return scope;
	}

	void setScope(AST::Node* node, SymbolScope* scope)
	{
		NodeInfo& info = this->astNodeMap[node];
		info.scope = scope;
	}

	SymbolScope* getScope(AST::Node* node)
	{
		const NodeInfo& info = this->astNodeMap[node];
		return info.scope;
	}

	void setCatchAllSymbolSource(AST::Node* node, CatchAllSymbolSource* ccsource)
	{
		this->astNodeMap[node].catchAllSource = ccsource;
	}

	CatchAllSymbolSource* getCatchAllSymbolSource(AST::Node* node)
	{
		return this->astNodeMap[node].catchAllSource;
	}

	void setSymbolSource(AST::Node* node, SymbolSource* source)
	{
		this->astNodeMap[node].symbolSource = source;
	}

	SymbolSource* getSymbolSource(AST::Node* node)
	{
		return this->astNodeMap[node].symbolSource;
	}

	void setSymbolDependency(AST::Node* node, SymbolDependency* dependency)
	{
		this->astNodeMap[node].symbolDependency = dependency;
	}

	SymbolDependency* getSymbolDependency(AST::Node* node)
	{
		return this->astNodeMap[node].symbolDependency;
	}	
};





