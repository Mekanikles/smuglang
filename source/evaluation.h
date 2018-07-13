
struct Value
{
	// TODO: Should be type ref or just type?
	//	Should not be able to affect this type by inference at this point
	TypeRef type;
	vector<char> data;
};

struct ExpressionEvaluator : AST::Visitor
{
	ExpressionEvaluator(Value* outValue)
		: outValue(outValue)
	{
		assert(outValue);
	}

	void visit(AST::StringLiteral* node) override
	{
		auto& val = *this->outValue;

		val.type = node->getType();

		string str = processQuotedInputString(node->value);

		auto length = str.length();
		val.data.resize(length);
		memcpy(val.data.data(), str.c_str(), length);

		this->success = true;
	}

	bool success = false;
	Value* outValue = nullptr;
};

bool evaluateExpression(AST::Expression* expr, Value* outValue)
{
	ExpressionEvaluator e(outValue);
	expr->accept(&e);

	return e.success;
}