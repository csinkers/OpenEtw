#pragma once
#include <cstdint>
// == Version 0.1.0 ==\\

enum class EtwInType
{
	UnicodeString, // A string of 16-bit codepoints in the UTF-16 encoding, corresponds to LPWSTR etc.
	AnsiString, // A string of 8-bit codepoints, typically in the current system codepage. TODO: Investigate viability of UTF-8 etc.
	Int8, Int16, Int32, Int64, // Signed integers of the usual sizes
	UInt8, UInt16, UInt32, UInt64, // Unsigned integers of the usual sizes
	Float, // A 32-bit IEEE-754 single-precision floating point number
	Double, // A 64-bit IEEE-754 double-precision floating point number
	Bool, // A boolean value
	Binary, // Binary data, i.e. a collection of bytes.
	Guid, // A GUID / UUID
	Pointer, // A pointer to a memory address
	FileTime, // A Windows FILETIME structure.
	SystemTime, // A Windows SYSTEMTIME structure.
	Sid, // A Windows SID structure.
	HexInt32,
	HexInt64
};

enum class EtwOutType
{
	String, Xml, ReducedString,
	Byte, UnsignedByte, HexInt8,
	Short, UnsignedShort, Port, HexInt16,
	Int, HResult, UnsignedInt, Pid, Tid, IPv4,
	EtwTime, ErrorCode, Win32Error, NtStatus, HexInt32,
	Long, UnsignedLong, HexInt64,
	Float, Double,
	Bool,
	HexBinary, IPv6, SocketAddress,
	Guid,
	DateTime, DateTimeCultureInsensitive
};

// ETW_PROVIDER(className, [name=], [prefix=], [guid=], [symbol=], [resourceFilename=], [messageFilename=], [opcodes=auto|manual])
#define ETW_PROVIDER_BEGIN(className, ...) \
class className \
{\
public:\
	static unsigned long Register();\
	static unsigned long Unregister();

#define ETW_PROVIDER_END };

// Declarations
#define ETW_KEYWORD(id, name, ...) // ETW_KEYWORD(id, name, [symbol=])
#define ETW_OPCODE(id, name, ...) // ETW_OPCODE(id, name, [symbol=], [message=])
#define ETW_CHANNEL(name, type, ...) // ETW_CHANNEL(name, type, [enabled="true|false"], [symbol=], [chid=], [isolation=])
#define ETW_LEVEL(id, name, ...) // ETW_LEVEL(id, name, [symbol=], [message=])
// Default opcodes: 
// Default keywords = CorrelationHint, AuditSuccess, AuditFailure, SQM, WDIDiag, WDIContext, ResponseTime
// Default channels = TraceClassic, System, Application, Security
// Default levels: Always, Critical, Error, Warning, Info, Verbose

//#define ETW_VERSION(version)
#define ETW_TASK_BEGIN(name, ...) // ETW_TASK_BEGIN(name, [id=], [guid=], [symbol=])
#define ETW_TASK_END

// Maps
#define ETW_BITMAP(name, ...)   enum name { // ETW_BITMAP(name, [prefixToIgnore=])
#define ETW_VALUEMAP(name, ...) enum name { // ETW_VALUEMAP(name, [prefixToIgnore=])
#define ETW_MAP_END };

// Parameters and implementation details
#define ETW_CUSTOM static void __cdecl
#define ETW_LEN(length)
#define ETW_COUNT(count)
#define ETW_IN(x)
#define ETW_OUT(x)
#define ETW_ACTIVITYID const GUID *
#define ETW_RELATED_ACTIVITYID const GUID *
//TODO: #define ETW_BIGSTRING(name) const char *

// ETW_EVENT(cppName, [id=], [name=], [symbol=], [message=], [keywords=], [level=], [opcode=], [channel=])
// Default levels: "win:LogAlways" "win:Critical" "win:Error" "win:Warning" "win:Informational" "win:Verbose" 
#define ETW_EVENT(cppName, ...) static void __cdecl cppName 

#define ETW_USE_STDAFX(...) // If this appears in the header, then the implementation will include the given filename, defaulting to "stdafx.h" if none is supplied.
