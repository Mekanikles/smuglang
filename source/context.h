#include <unordered_map>

struct Context
{
	vector<SymbolScope*> symbolScopes;

	struct NodeInfo
	{
		SymbolScope* scope = nullptr;

		// TODO: All nodes get these now
		SymbolSource* symbolSource = nullptr;
		SymbolDependency* symbolDependency = nullptr;

		// TODO: Only used during processing, store somewhere else
		CatchAllSymbolSource* catchAllSource = nullptr;
	};

	std::unordered_map<AST::Node*, NodeInfo> astNodeMap;
	std::unordered_map<AST::Node*, TypeRef> typeLiterals;

	void addTypeLiteral(AST::Node* node, TypeRef&& type)
	{
		this->typeLiterals[node] = std::move(type);
	}

	TypeRef& getTypeLiteral(AST::Node* node)
	{
		return this->typeLiterals[node];
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





