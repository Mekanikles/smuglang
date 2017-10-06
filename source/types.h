

struct Type
{
	bool isFunction = false;
	bool isInt = false;
	bool isFloat = false;
	bool isString = false;

	bool operator ==(const Type &b) const
	{
		return isFunction == b.isFunction &&
			isInt == b.isInt &&
			isFloat == b.isFloat &&
			isString == b.isString;
	}

	string toString()
	{
		return string("Type(Func: ") + std::to_string(isFunction) +
			", Int: " + std::to_string(isInt) +
			", Float: " + std::to_string(isFloat) +
			", Str: " + std::to_string(isString) + ")";
	}
};