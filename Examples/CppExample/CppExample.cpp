// CsEtw.Test.cpp : Defines the entry point for the console application.
//

#define CSETW_API
//#include "CsEtw/CsEtw.h"
#include "DemoProvider.h"
#include "MinimalProvider.h"

int main()
{
	PerformanceCounters::Register();
	// PerformanceCounters::PerfCounter(12, 15, 17, 25.4f);
	PerformanceCounters::PerfCounter(12, 15, 17);
//	MinimalProvider::Register();
//	MinimalProvider::MinMsg("Some message", 42);
//	MinimalProvider::Unregister();

//	DemoProvider::Register();
//
//	DemoProvider::EBlank();
//	DemoProvider::EAnsi("A test ansi string");
//	DemoProvider::EUnicode(L"A test Unicode string");
//	// DemoProvider::EFixedCount(L"Exactly 16 chars");
//	DemoProvider::EFixedLength(L"Exactly 32 chars are in this arg");
//	DemoProvider::ESByte('A');
//	DemoProvider::EUByte('A');
//
//	DemoProvider::ECustChannel();
//	DemoProvider::EDisabledChannel();
//	DemoProvider::EAnalyticChannel();
//	DemoProvider::EDebugChannel();
//	DemoProvider::ETraceClassicChan();
//	DemoProvider::ESystemChan();
//	DemoProvider::EAppChannel();
//	DemoProvider::ESecChannel();
//	DemoProvider::ECustLevel();
//	DemoProvider::ELogAlways();
//	DemoProvider::EFirstTask();
//	DemoProvider::ESecondTask();
//	DemoProvider::EKEvtClassic();
//	DemoProvider::EKCorrHint();
//	DemoProvider::EKeySmall();
//	DemoProvider::EKey1();
//	DemoProvider::EKeyAllDefault();
//
//    GUID guid = {0};
//    FILETIME filetime = {0};
//    SYSTEMTIME systemtime = {0};
//    SID sid = {0};
//
//	DemoProvider::EMany(
//		L"Unicode",
//		"ANSI",
//		'b',
//		0xff,
//		0x7fff,
//		0x8000,
//		0x10000000,
//		0xffffffff,
//		0x7fffffffffffffff,
//		0xffffffffffffffff,
//		0.5f,
//		3.1415926535897932384626433832795,
//		FALSE,
//		(BYTE*)"ACHTBYTE",
//		&guid,
//		(void *)nullptr,
//		&filetime,
//		&systemtime,
//		&sid,
//		0xdeadbeef,
//		0xbadc0de0f5addad);
//
//    BYTE buf[64];
//    for(int i=0;i<_countof(buf);i++)
//        buf[i] = i;
//
//	DemoProvider::ECountedBin(_countof(buf), buf);

    //BYTE buf2[32][4];
    //for(int i=0;i<32;i++)
    //    for(int j=0;j<4;j++)
    //        buf2[i][j] = (j << 6) | i;

	//DemoProvider::EBufArray(32, 4, (BYTE*)buf2);
//	DemoProvider::EBitMap(DemoProvider::CatLegs);
//	DemoProvider::EBitMap(DemoProvider::SpiderLegs);
//	DemoProvider::EBitMap((DemoProvider::BitMap1)(DemoProvider::Doubles | DemoProvider::CatLegs));
//	DemoProvider::EValueMap(DemoProvider::Val1);
//	DemoProvider::EValueMap(DemoProvider::Val2);
//	DemoProvider::EValueMap(DemoProvider::Meg);
//	DemoProvider::EValueMap(DemoProvider::Huge);

//	DemoProvider::Unregister();

//	auto channel = (void*)0x24587282;
//	auto operation =  (void*)0x49875232;
//	auto pipeHandle = (HANDLE)0x2e;
//	const char testMsg[] = "A test message";
//	auto msg = buffer(testMsg, sizeof(testMsg));
//	CsNpCreate(channel, _T("HostName"), _T("PipeName"), true);
//	CsNpPipeCreate(channel, operation, pipeHandle);
//	CsNpOperationCreate(channel, operation, pipeHandle, _T("Some operation"));
//	CsNpOperationProgress(channel, operation, pipeHandle, 1, 23);
//	CsNpOperationRemove(channel, operation, pipeHandle);
//	CsNpPipeReadInit(channel, operation, pipeHandle, 27);
//	CsNpPipeWriteInit(channel, operation, pipeHandle, msg);
//	CsNpEnqueueMessage(channel, operation, pipeHandle, msg);
//	CsNpError(channel, _T("An error message"), ERROR_ACCESS_DENIED, _T("Access denied"), _T(__FILE__), __LINE__);
//	CsNpDisconnected(channel, operation, pipeHandle);
//	CsNpPipeClose(channel, operation, pipeHandle);
    return 0;
}
