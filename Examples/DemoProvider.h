//**********************************************************************`
//* This is an include file generated by Message Compiler.             *`
//*                                                                    *`
//* Copyright (c) Microsoft Corporation. All Rights Reserved.          *`
//**********************************************************************`
#pragma once
#include <wmistr.h>
#include <evntrace.h>
#include "evntprov.h"
//
//  Initial Defs
//
#if !defined(ETW_INLINE)
#define ETW_INLINE DECLSPEC_NOINLINE __inline
#endif

#if defined(__cplusplus)
extern "C" {
#endif

//
// Allow Diasabling of code generation
//
#ifndef MCGEN_DISABLE_PROVIDER_CODE_GENERATION
#if  !defined(McGenDebug)
#define McGenDebug(a,b)
#endif 


#if !defined(MCGEN_TRACE_CONTEXT_DEF)
#define MCGEN_TRACE_CONTEXT_DEF
typedef struct _MCGEN_TRACE_CONTEXT
{
    TRACEHANDLE            RegistrationHandle;
    TRACEHANDLE            Logger;
    ULONGLONG              MatchAnyKeyword;
    ULONGLONG              MatchAllKeyword;
    ULONG                  Flags;
    ULONG                  IsEnabled;
    UCHAR                  Level; 
    UCHAR                  Reserve;
    USHORT                 EnableBitsCount;
    PULONG                 EnableBitMask;
    const ULONGLONG*       EnableKeyWords;
    const UCHAR*           EnableLevel;
} MCGEN_TRACE_CONTEXT, *PMCGEN_TRACE_CONTEXT;
#endif

#if !defined(MCGEN_LEVEL_KEYWORD_ENABLED_DEF)
#define MCGEN_LEVEL_KEYWORD_ENABLED_DEF
FORCEINLINE
BOOLEAN
McGenLevelKeywordEnabled(
    _In_ PMCGEN_TRACE_CONTEXT EnableInfo,
    _In_ UCHAR Level,
    _In_ ULONGLONG Keyword
    )
{
    //
    // Check if the event Level is lower than the level at which
    // the channel is enabled.
    // If the event Level is 0 or the channel is enabled at level 0,
    // all levels are enabled.
    //

    if ((Level <= EnableInfo->Level) || // This also covers the case of Level == 0.
        (EnableInfo->Level == 0)) {

        //
        // Check if Keyword is enabled
        //

        if ((Keyword == (ULONGLONG)0) ||
            ((Keyword & EnableInfo->MatchAnyKeyword) &&
             ((Keyword & EnableInfo->MatchAllKeyword) == EnableInfo->MatchAllKeyword))) {
            return TRUE;
        }
    }

    return FALSE;

}
#endif

#if !defined(MCGEN_EVENT_ENABLED_DEF)
#define MCGEN_EVENT_ENABLED_DEF
FORCEINLINE
BOOLEAN
McGenEventEnabled(
    _In_ PMCGEN_TRACE_CONTEXT EnableInfo,
    _In_ PCEVENT_DESCRIPTOR EventDescriptor
    )
{

    return McGenLevelKeywordEnabled(EnableInfo, EventDescriptor->Level, EventDescriptor->Keyword);

}
#endif


//
// EnableCheckMacro
//
#ifndef MCGEN_ENABLE_CHECK
#define MCGEN_ENABLE_CHECK(Context, Descriptor) (Context.IsEnabled &&  McGenEventEnabled(&Context, &Descriptor))
#endif

#if !defined(MCGEN_CONTROL_CALLBACK)
#define MCGEN_CONTROL_CALLBACK

DECLSPEC_NOINLINE __inline
VOID
__stdcall
McGenControlCallbackV2(
    _In_ LPCGUID SourceId,
    _In_ ULONG ControlCode,
    _In_ UCHAR Level,
    _In_ ULONGLONG MatchAnyKeyword,
    _In_ ULONGLONG MatchAllKeyword,
    _In_opt_ PEVENT_FILTER_DESCRIPTOR FilterData,
    _Inout_opt_ PVOID CallbackContext
    )
/*++

Routine Description:

    This is the notification callback for Vista.

Arguments:

    SourceId - The GUID that identifies the session that enabled the provider. 

    ControlCode - The parameter indicates whether the provider 
                  is being enabled or disabled.

    Level - The level at which the event is enabled.

    MatchAnyKeyword - The bitmask of keywords that the provider uses to 
                      determine the category of events that it writes.

    MatchAllKeyword - This bitmask additionally restricts the category 
                      of events that the provider writes. 

    FilterData - The provider-defined data.

    CallbackContext - The context of the callback that is defined when the provider 
                      called EtwRegister to register itself.

Remarks:

    ETW calls this function to notify provider of enable/disable

--*/
{
    PMCGEN_TRACE_CONTEXT Ctx = (PMCGEN_TRACE_CONTEXT)CallbackContext;
    ULONG Ix;
#ifndef MCGEN_PRIVATE_ENABLE_CALLBACK_V2
    UNREFERENCED_PARAMETER(SourceId);
    UNREFERENCED_PARAMETER(FilterData);
#endif

    if (Ctx == NULL) {
        return;
    }

    switch (ControlCode) {

        case EVENT_CONTROL_CODE_ENABLE_PROVIDER:
            Ctx->Level = Level;
            Ctx->MatchAnyKeyword = MatchAnyKeyword;
            Ctx->MatchAllKeyword = MatchAllKeyword;
            Ctx->IsEnabled = EVENT_CONTROL_CODE_ENABLE_PROVIDER;

            for (Ix = 0; Ix < Ctx->EnableBitsCount; Ix += 1) {
                if (McGenLevelKeywordEnabled(Ctx, Ctx->EnableLevel[Ix], Ctx->EnableKeyWords[Ix]) != FALSE) {
                    Ctx->EnableBitMask[Ix >> 5] |= (1 << (Ix % 32));
                } else {
                    Ctx->EnableBitMask[Ix >> 5] &= ~(1 << (Ix % 32));
                }
            }
            break;

        case EVENT_CONTROL_CODE_DISABLE_PROVIDER:
            Ctx->IsEnabled = EVENT_CONTROL_CODE_DISABLE_PROVIDER;
            Ctx->Level = 0;
            Ctx->MatchAnyKeyword = 0;
            Ctx->MatchAllKeyword = 0;
            if (Ctx->EnableBitsCount > 0) {
                RtlZeroMemory(Ctx->EnableBitMask, (((Ctx->EnableBitsCount - 1) / 32) + 1) * sizeof(ULONG));
            }
            break;
 
        default:
            break;
    }

#ifdef MCGEN_PRIVATE_ENABLE_CALLBACK_V2
    //
    // Call user defined callback
    //
    MCGEN_PRIVATE_ENABLE_CALLBACK_V2(
        SourceId,
        ControlCode,
        Level,
        MatchAnyKeyword,
        MatchAllKeyword,
        FilterData,
        CallbackContext
        );
#endif
   
    return;
}

#endif
#endif // MCGEN_DISABLE_PROVIDER_CODE_GENERATION
//+
// Provider TestProvider Event Count 29
//+
EXTERN_C __declspec(selectany) const GUID TPSYM = {0x04c40650, 0xceca, 0x4dd7, {0x8d, 0x79, 0xdc, 0x6d, 0x5a, 0x32, 0x5e, 0xdd}};

//
// Channel
//
#define ADCHAN 0x10
#define DISCHAN 0x11
#define ANALCHAN 0x12
#define DEBUGCHAN 0x13
#define TPSYM_CHANNEL_TraceClassic 0x0
#define TPSYM_CHANNEL_System 0x8
#define TPSYM_CHANNEL_Application 0x9
#define TPSYM_CHANNEL_Security 0xa

//
// Levels
//
#define CUSTLEV1 0x10

//
// Opcodes
//
#define CUSTOP1 0xa
#define CUSTOP2 0xb

//
// Tasks
//
#define FIRSTTASK 0x1
EXTERN_C __declspec(selectany) const GUID FirstTaskId = {0x02b9a26c, 0xba01, 0x4c84, {0xac, 0x49, 0xf4, 0x2c, 0xc1, 0x69, 0xa0, 0x9b}};
#define SECONDTSK 0x3
EXTERN_C __declspec(selectany) const GUID SecondTaskId = {0x8a561096, 0x325b, 0x4605, {0x85, 0xa6, 0xf5, 0x8a, 0xe9, 0xcc, 0xb0, 0x3b}};
//
// Keyword
//
#define KEY1 0x800000000000
#define KEYSMALL 0x1

//
// Event Descriptors
//
EXTERN_C __declspec(selectany) const EVENT_DESCRIPTOR EBlank = {0x1, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0};
#define EBlank_value 0x1
EXTERN_C __declspec(selectany) const EVENT_DESCRIPTOR EAnsi = {0x2, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0};
#define EAnsi_value 0x2
EXTERN_C __declspec(selectany) const EVENT_DESCRIPTOR EUnicode = {0x3, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0};
#define EUnicode_value 0x3
EXTERN_C __declspec(selectany) const EVENT_DESCRIPTOR EFixedCount = {0x4, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0};
#define EFixedCount_value 0x4
EXTERN_C __declspec(selectany) const EVENT_DESCRIPTOR EFixedLength = {0x5, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0};
#define EFixedLength_value 0x5
EXTERN_C __declspec(selectany) const EVENT_DESCRIPTOR ESByte = {0x6, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0};
#define ESByte_value 0x6
EXTERN_C __declspec(selectany) const EVENT_DESCRIPTOR EUByte = {0x7, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0};
#define EUByte_value 0x7
EXTERN_C __declspec(selectany) const EVENT_DESCRIPTOR ECustChannel = {0x8, 0x0, 0x10, 0x3, 0x0, 0x0, 0x8000000000000000};
#define ECustChannel_value 0x8
EXTERN_C __declspec(selectany) const EVENT_DESCRIPTOR EDisabledChannel = {0x9, 0x0, 0x11, 0x0, 0x0, 0x0, 0x4000000000000000};
#define EDisabledChannel_value 0x9
EXTERN_C __declspec(selectany) const EVENT_DESCRIPTOR EAnalyticChannel = {0xa, 0x0, 0x12, 0x0, 0x0, 0x0, 0x2000000000000000};
#define EAnalyticChannel_value 0xa
EXTERN_C __declspec(selectany) const EVENT_DESCRIPTOR EDebugChannel = {0xb, 0x0, 0x13, 0x0, 0x0, 0x0, 0x1000000000000000};
#define EDebugChannel_value 0xb
EXTERN_C __declspec(selectany) const EVENT_DESCRIPTOR ETraceClassicChan = {0xc, 0x0, 0x0, 0x0, 0x0, 0x0, 0x800000000000000};
#define ETraceClassicChan_value 0xc
EXTERN_C __declspec(selectany) const EVENT_DESCRIPTOR ESystemChan = {0xd, 0x0, 0x8, 0x4, 0x0, 0x0, 0x400000000000000};
#define ESystemChan_value 0xd
EXTERN_C __declspec(selectany) const EVENT_DESCRIPTOR EAppChannel = {0xe, 0x0, 0x9, 0x2, 0x0, 0x0, 0x200000000000000};
#define EAppChannel_value 0xe
EXTERN_C __declspec(selectany) const EVENT_DESCRIPTOR ESecChannel = {0xf, 0x0, 0xa, 0x1, 0x0, 0x0, 0x100000000000000};
#define ESecChannel_value 0xf
EXTERN_C __declspec(selectany) const EVENT_DESCRIPTOR ECustLevel = {0x10, 0x0, 0x0, 0x10, 0x0, 0x0, 0x0};
#define ECustLevel_value 0x10
EXTERN_C __declspec(selectany) const EVENT_DESCRIPTOR ELogAlways = {0x11, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0};
#define ELogAlways_value 0x11
EXTERN_C __declspec(selectany) const EVENT_DESCRIPTOR EFirstTask = {0x12, 0x0, 0x0, 0x0, 0x0, 0x1, 0x0};
#define EFirstTask_value 0x12
EXTERN_C __declspec(selectany) const EVENT_DESCRIPTOR ESecondTask = {0x13, 0x0, 0x0, 0x0, 0x0, 0x3, 0x0};
#define ESecondTask_value 0x13
EXTERN_C __declspec(selectany) const EVENT_DESCRIPTOR ENoTask = {0x14, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0};
#define ENoTask_value 0x14
EXTERN_C __declspec(selectany) const EVENT_DESCRIPTOR EKEvtClassic = {0x15, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0};
#define EKEvtClassic_value 0x15
EXTERN_C __declspec(selectany) const EVENT_DESCRIPTOR EKCorrHint = {0x16, 0x0, 0x0, 0x0, 0x0, 0x0, 0x40000000000000};
#define EKCorrHint_value 0x16
EXTERN_C __declspec(selectany) const EVENT_DESCRIPTOR EKeySmall = {0x17, 0x0, 0x0, 0x0, 0x0, 0x0, 0x1};
#define EKeySmall_value 0x17
EXTERN_C __declspec(selectany) const EVENT_DESCRIPTOR EKey1 = {0x18, 0x0, 0x0, 0x0, 0x0, 0x0, 0x800000000000};
#define EKey1_value 0x18
EXTERN_C __declspec(selectany) const EVENT_DESCRIPTOR EKeyAllDefault = {0x19, 0x0, 0x0, 0x0, 0x0, 0x0, 0x7f000000000000};
#define EKeyAllDefault_value 0x19
EXTERN_C __declspec(selectany) const EVENT_DESCRIPTOR EMany = {0x1a, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0};
#define EMany_value 0x1a
EXTERN_C __declspec(selectany) const EVENT_DESCRIPTOR ECountedBin = {0x1b, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0};
#define ECountedBin_value 0x1b
EXTERN_C __declspec(selectany) const EVENT_DESCRIPTOR EIntArray = {0x1c, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0};
#define EIntArray_value 0x1c
EXTERN_C __declspec(selectany) const EVENT_DESCRIPTOR EBufArray = {0x1d, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0};
#define EBufArray_value 0x1d

//
// Note on Generate Code from Manifest Windows Vista and above
//
//Structures :  are handled as a size and pointer pairs. The macro for the event will have an extra 
//parameter for the size in bytes of the structure. Make sure that your structures have no extra padding.
//
//Strings: There are several cases that can be described in the manifest. For array of variable length 
//strings, the generated code will take the count of characters for the whole array as an input parameter. 
//
//SID No support for array of SIDs, the macro will take a pointer to the SID and use appropriate 
//GetLengthSid function to get the length.
//

//
// Allow Diasabling of code generation
//
#ifndef MCGEN_DISABLE_PROVIDER_CODE_GENERATION

//
// Globals 
//


//
// Event Enablement Bits
//

EXTERN_C __declspec(selectany) DECLSPEC_CACHEALIGN ULONG TestProviderEnableBits[1];
EXTERN_C __declspec(selectany) const ULONGLONG TestProviderKeywords[14] = {0x0, 0x8000000000000000, 0x4000000000000000, 0x2000000000000000, 0x1000000000000000, 0x800000000000000, 0x400000000000000, 0x200000000000000, 0x100000000000000, 0x0, 0x40000000000000, 0x1, 0x800000000000, 0x7f000000000000};
EXTERN_C __declspec(selectany) const UCHAR TestProviderLevels[14] = {0, 3, 0, 0, 0, 0, 4, 2, 1, 16, 0, 0, 0, 0};
EXTERN_C __declspec(selectany) MCGEN_TRACE_CONTEXT TPSYM_Context = {0, 0, 0, 0, 0, 0, 0, 0, 14, TestProviderEnableBits, TestProviderKeywords, TestProviderLevels};

EXTERN_C __declspec(selectany) REGHANDLE TestProviderHandle = (REGHANDLE)0;

#if !defined(McGenEventRegisterUnregister)
#define McGenEventRegisterUnregister
DECLSPEC_NOINLINE __inline
ULONG __stdcall
McGenEventRegister(
    _In_ LPCGUID ProviderId,
    _In_opt_ PENABLECALLBACK EnableCallback,
    _In_opt_ PVOID CallbackContext,
    _Inout_ PREGHANDLE RegHandle
    )
/*++

Routine Description:

    This function register the provider with ETW USER mode.

Arguments:
    ProviderId - Provider Id to be register with ETW.

    EnableCallback - Callback to be used.

    CallbackContext - Context for this provider.

    RegHandle - Pointer to Registration handle.

Remarks:

    If the handle != NULL will return ERROR_SUCCESS

--*/
{
    ULONG Error;


    if (*RegHandle) {
        //
        // already registered
        //
        return ERROR_SUCCESS;
    }

    Error = EventRegister( ProviderId, EnableCallback, CallbackContext, RegHandle); 

    return Error;
}


DECLSPEC_NOINLINE __inline
ULONG __stdcall
McGenEventUnregister(_Inout_ PREGHANDLE RegHandle)
/*++

Routine Description:

    Unregister from ETW USER mode

Arguments:
            RegHandle this is the pointer to the provider context
Remarks:
            If Provider has not register RegHandle = NULL,
            return ERROR_SUCCESS
--*/
{
    ULONG Error;


    if(!(*RegHandle)) {
        //
        // Provider has not registerd
        //
        return ERROR_SUCCESS;
    }

    Error = EventUnregister(*RegHandle); 
    *RegHandle = (REGHANDLE)0;
    
    return Error;
}
#endif
//
// Register with ETW Vista +
//
#ifndef EventRegisterTestProvider
#define EventRegisterTestProvider() McGenEventRegister(&TPSYM, McGenControlCallbackV2, &TPSYM_Context, &TestProviderHandle) 
#endif

//
// UnRegister with ETW
//
#ifndef EventUnregisterTestProvider
#define EventUnregisterTestProvider() McGenEventUnregister(&TestProviderHandle) 
#endif

//
// Enablement check macro for EBlank
//

#define EventEnabledEBlank() ((TestProviderEnableBits[0] & 0x00000001) != 0)

//
// Event Macro for EBlank
//
#define EventWriteEBlank()\
        EventEnabledEBlank() ?\
        TemplateEventDescriptor(TestProviderHandle, &EBlank)\
        : ERROR_SUCCESS\

//
// Enablement check macro for EAnsi
//

#define EventEnabledEAnsi() ((TestProviderEnableBits[0] & 0x00000001) != 0)

//
// Event Macro for EAnsi
//
#define EventWriteEAnsi(Ansi)\
        EventEnabledEAnsi() ?\
        Template_s(TestProviderHandle, &EAnsi, Ansi)\
        : ERROR_SUCCESS\

//
// Enablement check macro for EUnicode
//

#define EventEnabledEUnicode() ((TestProviderEnableBits[0] & 0x00000001) != 0)

//
// Event Macro for EUnicode
//
#define EventWriteEUnicode(Unicode)\
        EventEnabledEUnicode() ?\
        Template_z(TestProviderHandle, &EUnicode, Unicode)\
        : ERROR_SUCCESS\

//
// Enablement check macro for EFixedCount
//

#define EventEnabledEFixedCount() ((TestProviderEnableBits[0] & 0x00000001) != 0)

//
// Event Macro for EFixedCount
//
//
// MC Note ::  Macro for Event id = 4
// For array of strings: needs to be in aconsecutive blob or memory
// and make sure you pass the count or characters in blob including null terminators
//
#define EventWriteEFixedCount(FixedCount_Len_, FixedCount)\
        EventEnabledEFixedCount() ?\
        Template_ZR(TestProviderHandle, &EFixedCount, FixedCount_Len_, FixedCount)\
        : ERROR_SUCCESS\

//
// Enablement check macro for EFixedLength
//

#define EventEnabledEFixedLength() ((TestProviderEnableBits[0] & 0x00000001) != 0)

//
// Event Macro for EFixedLength
//
#define EventWriteEFixedLength(FixedLen)\
        EventEnabledEFixedLength() ?\
        Template_z32(TestProviderHandle, &EFixedLength, FixedLen)\
        : ERROR_SUCCESS\

//
// Enablement check macro for ESByte
//

#define EventEnabledESByte() ((TestProviderEnableBits[0] & 0x00000001) != 0)

//
// Event Macro for ESByte
//
#define EventWriteESByte(Val)\
        EventEnabledESByte() ?\
        Template_c(TestProviderHandle, &ESByte, Val)\
        : ERROR_SUCCESS\

//
// Enablement check macro for EUByte
//

#define EventEnabledEUByte() ((TestProviderEnableBits[0] & 0x00000001) != 0)

//
// Event Macro for EUByte
//
#define EventWriteEUByte(Val)\
        EventEnabledEUByte() ?\
        Template_c(TestProviderHandle, &EUByte, Val)\
        : ERROR_SUCCESS\

//
// Enablement check macro for ECustChannel
//

#define EventEnabledECustChannel() ((TestProviderEnableBits[0] & 0x00000002) != 0)

//
// Event Macro for ECustChannel
//
#define EventWriteECustChannel()\
        EventEnabledECustChannel() ?\
        TemplateEventDescriptor(TestProviderHandle, &ECustChannel)\
        : ERROR_SUCCESS\

//
// Enablement check macro for EDisabledChannel
//

#define EventEnabledEDisabledChannel() ((TestProviderEnableBits[0] & 0x00000004) != 0)

//
// Event Macro for EDisabledChannel
//
#define EventWriteEDisabledChannel()\
        EventEnabledEDisabledChannel() ?\
        TemplateEventDescriptor(TestProviderHandle, &EDisabledChannel)\
        : ERROR_SUCCESS\

//
// Enablement check macro for EAnalyticChannel
//

#define EventEnabledEAnalyticChannel() ((TestProviderEnableBits[0] & 0x00000008) != 0)

//
// Event Macro for EAnalyticChannel
//
#define EventWriteEAnalyticChannel()\
        EventEnabledEAnalyticChannel() ?\
        TemplateEventDescriptor(TestProviderHandle, &EAnalyticChannel)\
        : ERROR_SUCCESS\

//
// Enablement check macro for EDebugChannel
//

#define EventEnabledEDebugChannel() ((TestProviderEnableBits[0] & 0x00000010) != 0)

//
// Event Macro for EDebugChannel
//
#define EventWriteEDebugChannel()\
        EventEnabledEDebugChannel() ?\
        TemplateEventDescriptor(TestProviderHandle, &EDebugChannel)\
        : ERROR_SUCCESS\

//
// Enablement check macro for ETraceClassicChan
//

#define EventEnabledETraceClassicChan() ((TestProviderEnableBits[0] & 0x00000020) != 0)

//
// Event Macro for ETraceClassicChan
//
#define EventWriteETraceClassicChan()\
        EventEnabledETraceClassicChan() ?\
        TemplateEventDescriptor(TestProviderHandle, &ETraceClassicChan)\
        : ERROR_SUCCESS\

//
// Enablement check macro for ESystemChan
//

#define EventEnabledESystemChan() ((TestProviderEnableBits[0] & 0x00000040) != 0)

//
// Event Macro for ESystemChan
//
#define EventWriteESystemChan()\
        EventEnabledESystemChan() ?\
        TemplateEventDescriptor(TestProviderHandle, &ESystemChan)\
        : ERROR_SUCCESS\

//
// Enablement check macro for EAppChannel
//

#define EventEnabledEAppChannel() ((TestProviderEnableBits[0] & 0x00000080) != 0)

//
// Event Macro for EAppChannel
//
#define EventWriteEAppChannel()\
        EventEnabledEAppChannel() ?\
        TemplateEventDescriptor(TestProviderHandle, &EAppChannel)\
        : ERROR_SUCCESS\

//
// Enablement check macro for ESecChannel
//

#define EventEnabledESecChannel() ((TestProviderEnableBits[0] & 0x00000100) != 0)

//
// Event Macro for ESecChannel
//
#define EventWriteESecChannel()\
        EventEnabledESecChannel() ?\
        TemplateEventDescriptor(TestProviderHandle, &ESecChannel)\
        : ERROR_SUCCESS\

//
// Enablement check macro for ECustLevel
//

#define EventEnabledECustLevel() ((TestProviderEnableBits[0] & 0x00000200) != 0)

//
// Event Macro for ECustLevel
//
#define EventWriteECustLevel()\
        EventEnabledECustLevel() ?\
        TemplateEventDescriptor(TestProviderHandle, &ECustLevel)\
        : ERROR_SUCCESS\

//
// Enablement check macro for ELogAlways
//

#define EventEnabledELogAlways() ((TestProviderEnableBits[0] & 0x00000001) != 0)

//
// Event Macro for ELogAlways
//
#define EventWriteELogAlways()\
        EventEnabledELogAlways() ?\
        TemplateEventDescriptor(TestProviderHandle, &ELogAlways)\
        : ERROR_SUCCESS\

//
// Enablement check macro for EFirstTask
//

#define EventEnabledEFirstTask() ((TestProviderEnableBits[0] & 0x00000001) != 0)

//
// Event Macro for EFirstTask
//
#define EventWriteEFirstTask()\
        EventEnabledEFirstTask() ?\
        TemplateEventDescriptor(TestProviderHandle, &EFirstTask)\
        : ERROR_SUCCESS\

//
// Enablement check macro for ESecondTask
//

#define EventEnabledESecondTask() ((TestProviderEnableBits[0] & 0x00000001) != 0)

//
// Event Macro for ESecondTask
//
#define EventWriteESecondTask()\
        EventEnabledESecondTask() ?\
        TemplateEventDescriptor(TestProviderHandle, &ESecondTask)\
        : ERROR_SUCCESS\

//
// Enablement check macro for ENoTask
//

#define EventEnabledENoTask() ((TestProviderEnableBits[0] & 0x00000001) != 0)

//
// Event Macro for ENoTask
//
#define EventWriteENoTask()\
        EventEnabledENoTask() ?\
        TemplateEventDescriptor(TestProviderHandle, &ENoTask)\
        : ERROR_SUCCESS\

//
// Enablement check macro for EKEvtClassic
//

#define EventEnabledEKEvtClassic() ((TestProviderEnableBits[0] & 0x00000001) != 0)

//
// Event Macro for EKEvtClassic
//
#define EventWriteEKEvtClassic()\
        EventEnabledEKEvtClassic() ?\
        TemplateEventDescriptor(TestProviderHandle, &EKEvtClassic)\
        : ERROR_SUCCESS\

//
// Enablement check macro for EKCorrHint
//

#define EventEnabledEKCorrHint() ((TestProviderEnableBits[0] & 0x00000400) != 0)

//
// Event Macro for EKCorrHint
//
#define EventWriteEKCorrHint()\
        EventEnabledEKCorrHint() ?\
        TemplateEventDescriptor(TestProviderHandle, &EKCorrHint)\
        : ERROR_SUCCESS\

//
// Enablement check macro for EKeySmall
//

#define EventEnabledEKeySmall() ((TestProviderEnableBits[0] & 0x00000800) != 0)

//
// Event Macro for EKeySmall
//
#define EventWriteEKeySmall()\
        EventEnabledEKeySmall() ?\
        TemplateEventDescriptor(TestProviderHandle, &EKeySmall)\
        : ERROR_SUCCESS\

//
// Enablement check macro for EKey1
//

#define EventEnabledEKey1() ((TestProviderEnableBits[0] & 0x00001000) != 0)

//
// Event Macro for EKey1
//
#define EventWriteEKey1()\
        EventEnabledEKey1() ?\
        TemplateEventDescriptor(TestProviderHandle, &EKey1)\
        : ERROR_SUCCESS\

//
// Enablement check macro for EKeyAllDefault
//

#define EventEnabledEKeyAllDefault() ((TestProviderEnableBits[0] & 0x00002000) != 0)

//
// Event Macro for EKeyAllDefault
//
#define EventWriteEKeyAllDefault()\
        EventEnabledEKeyAllDefault() ?\
        TemplateEventDescriptor(TestProviderHandle, &EKeyAllDefault)\
        : ERROR_SUCCESS\

//
// Enablement check macro for EMany
//

#define EventEnabledEMany() ((TestProviderEnableBits[0] & 0x00000001) != 0)

//
// Event Macro for EMany
//
#define EventWriteEMany(a, b, c, d, e, f, g, h, i, j, k, m, n, o, p, q, r, s, t, u, v)\
        EventEnabledEMany() ?\
        Template_zscclhdqixfgtbjpmykqx(TestProviderHandle, &EMany, a, b, c, d, e, f, g, h, i, j, k, m, n, o, p, q, r, s, t, u, v)\
        : ERROR_SUCCESS\

//
// Enablement check macro for ECountedBin
//

#define EventEnabledECountedBin() ((TestProviderEnableBits[0] & 0x00000001) != 0)

//
// Event Macro for ECountedBin
//
#define EventWriteECountedBin(len, data)\
        EventEnabledECountedBin() ?\
        Template_hb(TestProviderHandle, &ECountedBin, len, data)\
        : ERROR_SUCCESS\

//
// Enablement check macro for EIntArray
//

#define EventEnabledEIntArray() ((TestProviderEnableBits[0] & 0x00000001) != 0)

//
// Event Macro for EIntArray
//
#define EventWriteEIntArray(len, array)\
        EventEnabledEIntArray() ?\
        Template_cDR0(TestProviderHandle, &EIntArray, len, array)\
        : ERROR_SUCCESS\

//
// Enablement check macro for EBufArray
//

#define EventEnabledEBufArray() ((TestProviderEnableBits[0] & 0x00000001) != 0)

//
// Event Macro for EBufArray
//
#define EventWriteEBufArray(len, num, bufArray)\
        EventEnabledEBufArray() ?\
        Template_hcBR1(TestProviderHandle, &EBufArray, len, num, bufArray)\
        : ERROR_SUCCESS\

#endif // MCGEN_DISABLE_PROVIDER_CODE_GENERATION


//
// Allow Diasabling of code generation
//
#ifndef MCGEN_DISABLE_PROVIDER_CODE_GENERATION

//
// Template Functions 
//
//
//Template from manifest : TBlank
//
#ifndef TemplateEventDescriptor_def
#define TemplateEventDescriptor_def


ETW_INLINE
ULONG
TemplateEventDescriptor(
    _In_ REGHANDLE RegHandle,
    _In_ PCEVENT_DESCRIPTOR Descriptor
    )
{
    return EventWrite(RegHandle, Descriptor, 0, NULL);
}
#endif

//
//Template from manifest : TAnsi
//
#ifndef Template_s_def
#define Template_s_def
ETW_INLINE
ULONG
Template_s(
    _In_ REGHANDLE RegHandle,
    _In_ PCEVENT_DESCRIPTOR Descriptor,
    _In_opt_ LPCSTR  _Arg0
    )
{
#define ARGUMENT_COUNT_s 1

    EVENT_DATA_DESCRIPTOR EventData[ARGUMENT_COUNT_s];

    EventDataDescCreate(&EventData[0], 
                        (_Arg0 != NULL) ? _Arg0 : "NULL",
                        (_Arg0 != NULL) ? (ULONG)((strlen(_Arg0) + 1) * sizeof(CHAR)) : (ULONG)sizeof("NULL"));

    return EventWrite(RegHandle, Descriptor, ARGUMENT_COUNT_s, EventData);
}
#endif

//
//Template from manifest : TUnicode
//
#ifndef Template_z_def
#define Template_z_def
ETW_INLINE
ULONG
Template_z(
    _In_ REGHANDLE RegHandle,
    _In_ PCEVENT_DESCRIPTOR Descriptor,
    _In_opt_ PCWSTR  _Arg0
    )
{
#define ARGUMENT_COUNT_z 1

    EVENT_DATA_DESCRIPTOR EventData[ARGUMENT_COUNT_z];

    EventDataDescCreate(&EventData[0], 
                        (_Arg0 != NULL) ? _Arg0 : L"NULL",
                        (_Arg0 != NULL) ? (ULONG)((wcslen(_Arg0) + 1) * sizeof(WCHAR)) : (ULONG)sizeof(L"NULL"));

    return EventWrite(RegHandle, Descriptor, ARGUMENT_COUNT_z, EventData);
}
#endif

//
//Template from manifest : TFixedCount
//
#ifndef Template_ZR_def
#define Template_ZR_def
ETW_INLINE
ULONG
Template_ZR(
    _In_ REGHANDLE RegHandle,
    _In_ PCEVENT_DESCRIPTOR Descriptor,
    _In_ ULONG _Arg0_Len_,
    _In_reads_(_Arg0_Len_) PCWSTR  _Arg0
    )
{
#define ARGUMENT_COUNT_ZR 1

    EVENT_DATA_DESCRIPTOR EventData[ARGUMENT_COUNT_ZR];

    EventDataDescCreate(&EventData[0], _Arg0, (ULONG)(sizeof(WCHAR)*_Arg0_Len_));

    return EventWrite(RegHandle, Descriptor, ARGUMENT_COUNT_ZR, EventData);
}
#endif

//
//Template from manifest : TFixedLength
//
#ifndef Template_z32_def
#define Template_z32_def
ETW_INLINE
ULONG
Template_z32(
    _In_ REGHANDLE RegHandle,
    _In_ PCEVENT_DESCRIPTOR Descriptor,
    _In_reads_(32) PCWCH  _Arg0
    )
{
#define ARGUMENT_COUNT_z32 1

    EVENT_DATA_DESCRIPTOR EventData[ARGUMENT_COUNT_z32];

    EventDataDescCreate(&EventData[0], _Arg0, (ULONG)(sizeof(WCHAR)*32));

    return EventWrite(RegHandle, Descriptor, ARGUMENT_COUNT_z32, EventData);
}
#endif

//
//Template from manifest : TSByte
//
#ifndef Template_c_def
#define Template_c_def
ETW_INLINE
ULONG
Template_c(
    _In_ REGHANDLE RegHandle,
    _In_ PCEVENT_DESCRIPTOR Descriptor,
    _In_ const char  _Arg0
    )
{
#define ARGUMENT_COUNT_c 1

    EVENT_DATA_DESCRIPTOR EventData[ARGUMENT_COUNT_c];

    EventDataDescCreate(&EventData[0], &_Arg0, sizeof(const char)  );

    return EventWrite(RegHandle, Descriptor, ARGUMENT_COUNT_c, EventData);
}
#endif

//
//Template from manifest : TMany
//
#ifndef Template_zscclhdqixfgtbjpmykqx_def
#define Template_zscclhdqixfgtbjpmykqx_def
ETW_INLINE
ULONG
Template_zscclhdqixfgtbjpmykqx(
    _In_ REGHANDLE RegHandle,
    _In_ PCEVENT_DESCRIPTOR Descriptor,
    _In_opt_ PCWSTR  _Arg0,
    _In_opt_ LPCSTR  _Arg1,
    _In_ const char  _Arg2,
    _In_ const UCHAR  _Arg3,
    _In_ const signed short  _Arg4,
    _In_ const unsigned short  _Arg5,
    _In_ const signed int  _Arg6,
    _In_ const unsigned int  _Arg7,
    _In_ signed __int64  _Arg8,
    _In_ unsigned __int64  _Arg9,
    _In_ const float  _Arg10,
    _In_ const double  _Arg11,
    _In_ const BOOL  _Arg12,
    _In_reads_(8) const BYTE*  _Arg13,
    _In_ LPCGUID  _Arg14,
    _In_opt_ const void *  _Arg15,
    _In_ const FILETIME*  _Arg16,
    _In_ const SYSTEMTIME*  _Arg17,
    _In_ const SID *  _Arg18,
    _In_ const signed int  _Arg19,
    _In_ signed __int64  _Arg20
    )
{
#define ARGUMENT_COUNT_zscclhdqixfgtbjpmykqx 21

    EVENT_DATA_DESCRIPTOR EventData[ARGUMENT_COUNT_zscclhdqixfgtbjpmykqx];

    EventDataDescCreate(&EventData[0], 
                        (_Arg0 != NULL) ? _Arg0 : L"NULL",
                        (_Arg0 != NULL) ? (ULONG)((wcslen(_Arg0) + 1) * sizeof(WCHAR)) : (ULONG)sizeof(L"NULL"));

    EventDataDescCreate(&EventData[1], 
                        (_Arg1 != NULL) ? _Arg1 : "NULL",
                        (_Arg1 != NULL) ? (ULONG)((strlen(_Arg1) + 1) * sizeof(CHAR)) : (ULONG)sizeof("NULL"));

    EventDataDescCreate(&EventData[2], &_Arg2, sizeof(const char)  );

    EventDataDescCreate(&EventData[3], &_Arg3, sizeof(const UCHAR)  );

    EventDataDescCreate(&EventData[4], &_Arg4, sizeof(const signed short)  );

    EventDataDescCreate(&EventData[5], &_Arg5, sizeof(const unsigned short)  );

    EventDataDescCreate(&EventData[6], &_Arg6, sizeof(const signed int)  );

    EventDataDescCreate(&EventData[7], &_Arg7, sizeof(const unsigned int)  );

    EventDataDescCreate(&EventData[8], &_Arg8, sizeof(signed __int64)  );

    EventDataDescCreate(&EventData[9], &_Arg9, sizeof(unsigned __int64)  );

    EventDataDescCreate(&EventData[10], &_Arg10, sizeof(const float)  );

    EventDataDescCreate(&EventData[11], &_Arg11, sizeof(const double)  );

    EventDataDescCreate(&EventData[12], &_Arg12, sizeof(const BOOL)  );

    EventDataDescCreate(&EventData[13], _Arg13, (ULONG)sizeof(char)*8);

    EventDataDescCreate(&EventData[14], _Arg14, sizeof(GUID)  );

    EventDataDescCreate(&EventData[15], &_Arg15, sizeof(PVOID)  );

    EventDataDescCreate(&EventData[16], _Arg16, sizeof(FILETIME)  );

    EventDataDescCreate(&EventData[17], _Arg17, sizeof(SYSTEMTIME)  );

    EventDataDescCreate(&EventData[18], _Arg18, GetLengthSid((PSID)_Arg18));

    EventDataDescCreate(&EventData[19], &_Arg19, sizeof(const signed int)  );

    EventDataDescCreate(&EventData[20], &_Arg20, sizeof(signed __int64)  );

    return EventWrite(RegHandle, Descriptor, ARGUMENT_COUNT_zscclhdqixfgtbjpmykqx, EventData);
}
#endif

//
//Template from manifest : TCountedBin
//
#ifndef Template_hb_def
#define Template_hb_def
ETW_INLINE
ULONG
Template_hb(
    _In_ REGHANDLE RegHandle,
    _In_ PCEVENT_DESCRIPTOR Descriptor,
    _In_ const unsigned short  _Arg0,
    _In_reads_(_Arg0) const BYTE*  _Arg1
    )
{
#define ARGUMENT_COUNT_hb 2

    EVENT_DATA_DESCRIPTOR EventData[ARGUMENT_COUNT_hb];

    EventDataDescCreate(&EventData[0], &_Arg0, sizeof(const unsigned short)  );

    EventDataDescCreate(&EventData[1], _Arg1, (ULONG)sizeof(char)*_Arg0);

    return EventWrite(RegHandle, Descriptor, ARGUMENT_COUNT_hb, EventData);
}
#endif

//
//Template from manifest : TIntArray
//
#ifndef Template_cDR0_def
#define Template_cDR0_def
ETW_INLINE
ULONG
Template_cDR0(
    _In_ REGHANDLE RegHandle,
    _In_ PCEVENT_DESCRIPTOR Descriptor,
    _In_ const UCHAR  _Arg0,
    _In_reads_(_Arg0) const signed int *_Arg1
    )
{
#define ARGUMENT_COUNT_cDR0 2

    EVENT_DATA_DESCRIPTOR EventData[ARGUMENT_COUNT_cDR0];

    EventDataDescCreate(&EventData[0], &_Arg0, sizeof(const UCHAR)  );

    EventDataDescCreate(&EventData[1],  _Arg1, sizeof(const signed int)*_Arg0);

    return EventWrite(RegHandle, Descriptor, ARGUMENT_COUNT_cDR0, EventData);
}
#endif

//
//Template from manifest : TBufArray
//
#ifndef Template_hcBR1_def
#define Template_hcBR1_def
ETW_INLINE
ULONG
Template_hcBR1(
    _In_ REGHANDLE RegHandle,
    _In_ PCEVENT_DESCRIPTOR Descriptor,
    _In_ const unsigned short  _Arg0,
    _In_ const UCHAR  _Arg1,
    _In_reads_(_Arg0*_Arg1) const BYTE*  _Arg2
    )
{
#define ARGUMENT_COUNT_hcBR1 3

    EVENT_DATA_DESCRIPTOR EventData[ARGUMENT_COUNT_hcBR1];

    EventDataDescCreate(&EventData[0], &_Arg0, sizeof(const unsigned short)  );

    EventDataDescCreate(&EventData[1], &_Arg1, sizeof(const UCHAR)  );

    EventDataDescCreate(&EventData[2], _Arg2, (ULONG)sizeof(char)*_Arg0*_Arg1);

    return EventWrite(RegHandle, Descriptor, ARGUMENT_COUNT_hcBR1, EventData);
}
#endif

#endif // MCGEN_DISABLE_PROVIDER_CODE_GENERATION

#if defined(__cplusplus)
};
#endif

#define MSG_TestProvider_event_13_message    0x0000000DL
#define MSG_TestProvider_event_14_message    0x0000000EL
#define MSG_TestProvider_event_15_message    0x0000000FL
#define MSG_TestProvider_Keyword_KEYSMALL_message 0x10000001L
#define MSG_TestProvider_Keyword_KEY1_message 0x10000030L
#define MSG_keyword_ResponseTime             0x10000031L
#define MSG_keyword_WDIContext               0x10000032L
#define MSG_keyword_WDIDiag                  0x10000033L
#define MSG_keyword_SQM                      0x10000034L
#define MSG_keyword_AuditFailure             0x10000035L
#define MSG_keyword_AuditSuccess             0x10000036L
#define MSG_keyword_CorrelationHint          0x10000037L
#define MSG_TestProvider_opcode_CUSTOP1_message 0x3000000AL
#define MSG_TestProvider_opcode_CUSTOP2_message 0x3000000BL
#define MSG_level_LogAlways                  0x50000000L
#define MSG_level_Critical                   0x50000001L
#define MSG_level_Error                      0x50000002L
#define MSG_level_Warning                    0x50000003L
#define MSG_level_Informational              0x50000004L
#define MSG_TestProvider_level_CUSTLEV1_message 0x50000010L
#define MSG_task_None                        0x70000000L
#define MSG_TestProvider_task_FIRSTTASK_message 0x70000001L
#define MSG_TestProvider_task_SECONDTSK_message 0x70000003L
#define MSG_TestProvider_channel_ADCHAN_message 0x90000001L
#define MSG_TestProvider_channel_DISCHAN_message 0x90000002L
#define MSG_TestProvider_channel_ANALCHAN_message 0x90000003L
#define MSG_TestProvider_channel_DEBUGCHAN_message 0x90000004L
#define MSG_channel_TraceClassic             0x90000005L
#define MSG_channel_System                   0x90000006L
#define MSG_channel_Application              0x90000007L
#define MSG_channel_Security                 0x90000008L
#define MSG_TestProvider_event_8_message     0xB0000008L
#define MSG_TestProvider_event_26_message    0xB000001AL
#define MSG_TestProvider_event_27_message    0xB000001BL
#define MSG_TestProvider_event_28_message    0xB000001CL
#define MSG_TestProvider_event_29_message    0xB000001DL
#define MSG_TestProvider_map_ValMap1_1_message 0xD0000001L
#define MSG_TestProvider_map_ValMap1_2_message 0xD0000002L
#define MSG_TestProvider_map_ValMap1_1048576_message 0xD0000003L
#define MSG_TestProvider_map_ValMap1_2147483647_message 0xD0000004L
#define MSG_TestProvider_map_BitMap1_1_message 0xF0000001L
#define MSG_TestProvider_map_BitMap1_2_message 0xF0000002L
#define MSG_TestProvider_map_BitMap1_4_message 0xF0000003L
#define MSG_TestProvider_map_BitMap1_8_message 0xF0000004L
