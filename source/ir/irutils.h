#pragma once

#include "core.h"
#include "ir/ir.h"

i64 convertLiteralToInt64(IR::Literal& literal)
{
	auto& primitive = literal.getType()->getPrimitive();
	assert(primitive.isConcrete() && primitive.isInteger());
	
	if (primitive.Signed)
	{
		switch (primitive.size)
		{
			case 8: return literal.readValue<i8>();
			case 16: return literal.readValue<i16>();
			case 32: return literal.readValue<i32>();
			case 64: return literal.readValue<i64>();
			default:
				assert("int size not supported in evaluation");
		}
	}
	else
	{
		switch (primitive.size)
		{
			case 8: return literal.readValue<u8>();
			case 16: return literal.readValue<u16>();
			case 32: return literal.readValue<u32>();
			case 64:
			{
				auto uvalue = literal.readValue<u64>();
				assert(uvalue < (u64)INT64_MAX);
				return uvalue;
			}
			default:
				assert("int size not supported in evaluation");
		}
	}

	return -1;
}