#pragma once
#include "../../etw.h"
ETW_HEADER(<cstdint>)

//ETW_PROVIDER_BEGIN(MinimalProvider, prefix="Min")

//ETW_TASK_BEGIN(1, Msg)
//	ETW_EVENT(MinMsg)(char *text, int num);
//ETW_TASK_END
ETW_PROVIDER_BEGIN(PerformanceCounters)

ETW_TASK_BEGIN(1, BlaTask)
	ETW_EVENT(PerfCounter)(int category, int instance, int counter);
ETW_TASK_END

ETW_PROVIDER_END
