#pragma once
#include "../../etw.h"

// TODO: Generation options, e.g. auto-gen task per event, auto-populate messages for various object types etc
// TODO: Add checks to ensure various ids are unique, including the default tasks, opcodes, keywords and levels.
// TODO: Improve error messages and context reporting, stop spewing stack traces.
ETW_HEADER(<cstdint>)

ETW_PROVIDER_BEGIN(DemoProvider)
	// TODO: Generate per-event task names? Allow overloading so we can handle start/stop pairs

	// Level CustLevel1 CUSTLEV1 16
	ETW_LEVEL(16, CustLevel1, symbol="CUSTLEV1", message="A custom level")

	// Only use start and stop if possible.
	ETW_OPCODE(11, CustOp1, symbol="CUSTOP1", message="A custom opcode")
	ETW_OPCODE(12, CustOp2, symbol="CUSTOP2", message="A second custom opcode")

	// Note: Only use keywords for verbose events
	ETW_KEYWORD(0x080000000000, Keyword1, symbol="KEY1", message="A custom keyword")
	ETW_KEYWORD(0x100, KeySmall, symbol="KEYSMALL", message="Another custom keyword with a value of 1")

	ETW_BITMAP(BitMap1)
		Odd = 1,
		Doubles = 2,
		CatLegs = 4,
		SpiderLegs = 8
	ETW_MAP_END

	ETW_VALUEMAP(ValMap1)
		Val1 = 1,
		Val2 = 2,
		Meg = 1048576,
		Huge = 0x7fffffff
	ETW_MAP_END

	ETW_EVENT(EBlank)();
	ETW_EVENT(EAnsi)(const char *Ansi);
	ETW_EVENT(EUnicode)(const wchar_t *Unicode);
	// ETW_EVENT(EFixedCount)(const wchar_t *FixedCount ETW_COUNT(16));
	ETW_EVENT(EFixedLength)(const wchar_t *FixedLen ETW_LEN(32));
	ETW_EVENT(ESByte)(char Val);
	ETW_EVENT(EUByte)(unsigned char Val);

	ETW_CHANNEL(CustChannel, Admin, symbol="ADCHAN", message="An enabled admin channel")
	ETW_CHANNEL(DisabledChannel, Operational, symbol="DISCHAN", enabled="false", message="A disabled operational channel")
	ETW_CHANNEL(AnalyticChannel, Analytic, symbol="ANALCHAN", isolation="Application", message="An enabled analytic channel with Application Isolation security")
	ETW_CHANNEL(DebugChannel, Debug, symbol="DEBUGCHAN", isolation="System", message="An enabled debug channel with system isolation security")

	ETW_EVENT(ECustChannel, level="win:Warning", channel="CustChannel", message="Required message for Admin event (level also required)")();
	ETW_EVENT(EDisabledChannel, channel="DisabledChannel")  ();
	ETW_EVENT(EAnalyticChannel, channel="AnalyticChannel") ();
	ETW_EVENT(EDebugChannel, channel="DebugChannel")    ();
	ETW_EVENT(ETraceClassicChan, channel="TraceClassic")    ();
	ETW_EVENT(ESystemChan, level="win:Informational", channel="System", message="Is Admin event") ();
	ETW_EVENT(EAppChannel, level="win:Error", channel="Application", message="Is App channel")    ();
	ETW_EVENT(ESecChannel, level="win:Critical", channel="Security", message="Is Security event") ();
//
//	ETW_EVENT(ECustLevel, level="CustLevel1") ();
//	ETW_EVENT(ELogAlways, level="win:LogAlways") ();
//
//	ETW_TASK_BEGIN(1, FirstTask, guid="{02B9A26C-BA01-4C84-AC49-F42CC169A09B}", symbol="FIRSTTASK", message="A task")
//		ETW_EVENT(EFirstTask)();
//	ETW_TASK_END
//
//	ETW_TASK_BEGIN(3, SecondTask, guid="{8A561096-325B-4605-85A6-F58AE9CCB03B}", symbol="SECONDTSK", message="A second task but with an id of 3")
//		ETW_EVENT(ESecondTask)();
//	ETW_TASK_END
//
//	// ETW_EVENT(20) ENoTask  task="win:None"();
//	ETW_EVENT(EKEvtClassic)();
//	ETW_EVENT(EKCorrHint, keywords="win:CorrelationHint")();
//	ETW_EVENT(EKeySmall, keywords="KeySmall")();
//	ETW_EVENT(EKey1, keywords="Keyword1") ();
//	ETW_EVENT(EKeyAllDefault, keywords="win:CorrelationHint, win:AuditSuccess, win:AuditFailure, win:SQM, win:WDIDiag, win:WDIContext, win:ResponseTime") ();

//	ETW_EVENT(26, message="An event with many parameters") EMany(
//		const wchar_t *a,
//		const char *b,
//		char c,
//		unsigned char d,
//		short e,
//		unsigned short f,
//		int g,
//		unsigned int h,
//		long long i,
//		unsigned long long j,
//		float k,
//		double l,
//		BOOL m ETW_CAST(0 != m),
//		BYTE *n ETW_LEN(8),
//		GUID *o,
//		void *p,
//		FILETIME *q,
//		SYSTEMTIME *r,
//		SID *s,
//		int ETW_IN(EtwInType::HexInt32) ETW_OUT(EtwOutType::HexInt32) t,
//		long long ETW_IN(EtwInType::HexInt64) ETW_OUT(EtwOutType::HexInt64) u);
//
//	ETW_EVENT(27, message="An event that takes a counted binary parameter") ECountedBin(
//		unsigned short len,
//		const BYTE *data ETW_LEN(len));

	//ETW_EVENT(28, message="An event that takes an array of ints") EIntArray(
	//	unsigned char len,
	//	int *array ETW_COUNT(len));

	//ETW_EVENT(29, message="An event that takes an array of binary buffers") EBufArray(
	//	unsigned short len,
	//	unsigned char num,
	//	BYTE *bufArray ETW_COUNT(num) ETW_LEN(len));

	ETW_EVENT(EBitMap)(BitMap1 map);
	ETW_EVENT(EValueMap)(ValMap1 map);
ETW_PROVIDER_END

#if 0
ETW_POSTSCRIPT_BEGIN

// TODO: Implement postscript directives

ETW_POSTSCRIPT_END
#endif
