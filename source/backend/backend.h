#pragma once

#include "core.h"
#include "value.h"
#include "ir.h"
#include "types.h"

#include "backend/context.h"
#include "backend/generator.h"

namespace Backend
{

bool g_backendInitialized = false;
void ensureBackendIsInitialized()
{
	// Init llvm stuff
	if (!g_backendInitialized)
	{
		llvm::InitializeAllTargetInfos();
		llvm::InitializeAllTargets();
		llvm::InitializeAllTargetMCs();
		llvm::InitializeAllAsmParsers();
		llvm::InitializeAllAsmPrinters();
		g_backendInitialized = true;
	}
}

}