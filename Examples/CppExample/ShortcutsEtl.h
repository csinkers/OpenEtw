#ifndef _SHORTCUTS_ETL_H_
#define _SHORTCUTS_ETL_H_

#include "../../etw.h"
#include "Windows.h"

// ShortcutsEtl.h: This file defines the common API for ShortcutsETL.
// This is implemented by ShortcutsETL/ShortcutsETL.cpp and also by a ShortcutsEtlWrapper.cpp for calling code.

// The following ifdef block is the standard way of creating macros which make exporting
// from a DLL simpler. All files within this DLL are compiled with the SHORTCUTSETL_EXPORTS
// symbol defined on the command line. This symbol should not be defined on any project
// that uses this DLL. This way any other project whose source files include this file see
// SHORTCUTSETL_API functions as being imported from a DLL, whereas this DLL sees symbols
// defined with this macro as being exported.
#ifdef SHORTCUTSETL_EXPORTS
#define SHORTCUTSETL_API __declspec(dllexport)
#elif SCTOOLS_EXPORTS
// ScTools defines a wrapper implementation in ShortcutsEtlWrapper.
// These wrapper functions call the actual ShortcutsEtl.dll via dynamic means
// In order for other components of the program (e.g. Shortcuts.exe, ScLegacy.dll)
// to access them, these wrapper functions must also be exported from ScTools.
#define SHORTCUTSETL_API __declspec(dllexport)
#else
#define SHORTCUTSETL_API __declspec(dllimport)
#endif

typedef void(__cdecl *tScEtlStatusChangeCallback)(bool active);

SHORTCUTSETL_API ETW_PROVIDER_BEGIN(ScEtl, name="ShortcutsETL")

	bool  __cdecl IsKeywordEnabled(uint64_t keyword);
	int   __cdecl ScEtlGetNextCorrelationId();
	float __cdecl ScEtlQPCToMS(__int64 ticks);

	void RegisterMessageHook();

    // This needs to be kept in sync with shared/Dependencies/DomainTrace.h and backward + forward compatability kept in mind.
    enum ScEtlDomainTraceEventType {
        DomainWarning         = 0,
        DomainError           = 1,
        UnitOfWorkBegin       = 2,
        UnitOfWorkCommitStart = 3,
        UnitOfWorkCommitStop  = 4,
        UnitOfWorkAbortStart  = 5,
        UnitOfWorkAbortStop   = 6,
        UnitOfWorkInsert      = 7,
        UnitOfWorkUpdate      = 8,
        UnitOfWorkDelete      = 9
    };

    ETW_BITMAP(LogLevel)
        Critical    = 1,
        Error       = 2,
        Warning     = 4,
        Information = 8,
        Verbose     = 16
    ETW_MAP_END

    ETW_KEYWORD(0x1,  Misc)
    ETW_KEYWORD(0x2,  MustTraceStack)
    ETW_KEYWORD(0x4,  NetworkTrace)
    ETW_KEYWORD(0x8,  Logging)
    ETW_KEYWORD(0x10, Payments)
    ETW_KEYWORD(0x20, GuiDump)
    ETW_KEYWORD(0x40, GuiDumpText)
    ETW_KEYWORD(0x80, MsgHook)
    ETW_KEYWORD(0x100,Json)
    ETW_KEYWORD(0x200,WmCreate)
    // If this keyword is active, a crash will occur instead of showing the user an error window when an exception is caught.
    // This is intended for use with the ShortcutsDiagnostics tracing tool, the crash will trigger data collection.
    ETW_KEYWORD(0x400, AvOnCaughtException)
    // If this keyword is active, then no crash will occur for non-user visible caught exceptions
    // (i.e. those that only write to the log instead of showing an error message to the user)
    // If AvOnCaughtException is not active then this keyword has no effect.
    ETW_KEYWORD(0x800, AvSuppressLogOnly)
    ETW_KEYWORD(0x1000,ForceVerboseLogging)
    ETW_KEYWORD(0x2000,DomainTrace)
    ETW_KEYWORD(0x4000,Statistics)
    // If this keyword is active, statistics will be
    // emitted every 10 seconds. If inactive, every second.
    ETW_KEYWORD(0x8000, SlowStatistics)

    ETW_BITMAP(LogTableType, prefixToIgnore="LT")
        LTBadPassword  = 0x1,
        LTStockControl = 0x2,
        LTAppointments = 0x4,
        LTPos          = 0x8,
        LTScreenAccess = 0x10,
        LTReports      = 0x20,
        LTSystem       = 0x40,
        LTClients      = 0x80,
        LTStylists     = 0x100,
        LTInternet     = 0x200,
        LTEndOfDay     = 0x400,
        LTRoster       = 0x800,
        LTStartOfDay   = 0x1000,
        LTClientMerge  = 0x2000
    ETW_MAP_END

    ETW_BITMAP(PersistenceEventType)
        Saved            = 0x1,
        Updated          = 0x2,
        Deleted          = 0x4,
        ResetEntireTable = 0x8
    ETW_MAP_END

    // This should correspond to PessimisticLockStatus in PessimisticLock.h as
    // well as the valueMap & strings in ShortcutsEtl.man
    ETW_VALUEMAP(LockStatus)
        Unknown           = 0, // The status before a lock is attempted
        Acquired          = 1, // The lock was successfully acquired, as the lock was not taken by any other instance
        AcquiredStaleLock = 2, // The lock was successfully acquired as the existing holder was stale (their SQL connection was no longer active)
        ReacquiredOwnLock = 3, // This session already owned the lock, but requested it again. These locks are not currently re-entrant, so this usually indicates a bug.
        LockedByOther     = 4, // The lock was not acquired/released due to another instance holding the lock
        Released          = 5, // The lock was released
        WasNotLocked      = 6, // An attempt was made to release a non-existent lock
        Errored           = 7  // An error occurred when taking or releasing the lock
    ETW_MAP_END

    ETW_VALUEMAP(HitTest, prefixToIgnore="HT")
        HTNowhere     = 0,
        HTClient      = 1,
        HTCaption     = 2,
        HTSysMenu     = 3,
        HTSize        = 4,
        HTMenu        = 5,
        HTHScroll     = 6,
        HTVScroll     = 7,
        HTMinButton   = 8,
        HTMaxButton   = 9,
        HTLeft        = 10,
        HTRight       = 11,
        HTTop         = 12,
        HTTopLeft     = 13,
        HTTopRight    = 14,
        HTBottom      = 15,
        HTBottomLeft  = 16,
        HTBottomRight = 17,
        HTBorder      = 18,
        HTClose       = 20,
        HTHelp        = 21
    ETW_MAP_END

    ETW_VALUEMAP(WmSysCommand)
        IsSecure    = 1,
        Size         = 61440,
        Move         = 61456,
        Minimize     = 61472,
        Maximize     = 61488,
        NextWindow   = 61504,
        PrevWindow   = 61520,
        Close        = 61536,
        VScroll      = 61552,
        HScroll      = 61568,
        MouseMenu    = 61584,
        KeyMenu      = 61696,
        Restore      = 61728,
        Tasklist     = 61744,
        Screensave   = 61760,
        Hotkey       = 61776,
        Default      = 61792,
        MonitorPower = 61808,
        ContextHelp  = 61824
    ETW_MAP_END

	ETW_VALUEMAP(Category, prefixToIgnore="C")
		CVisitScheduling    = 1,
		CComEvents          = 2,
		CWarning            = 3,
		CNetworkMsg         = 4,
		CImageCacheLoad     = 5,
		CPayments           = 6,
		CVisits             = 7,
		CPointOfSale        = 8,
		CGiftCardRedemption = 9,
		CAppointments       = 10,
		CLogicalOperation   = 11
	ETW_MAP_END

    ETW_VALUEMAP_FROM_FILE(VKs, ShortcutsEtlKeys.csv)
    ETW_VALUEMAP_FROM_FILE(WMs, ShortcutsEtlMessages.csv)

    ETW_VALUEMAP(ImageSource)
        Employee    = 1,
        SystemTable = 2,
        IconImages  = 3
    ETW_MAP_END

    ETW_VALUEMAP(PersistenceClientEvent)
        Connect      = 1,
        Connected    = 2,
        Disconnect   = 3,
        Disconnected = 4,
        EnqueueMain  = 5,
        DiscardMain  = 6,
        DiscardLocal = 7,
        MoveToLocal  = 8,
        Send         = 9
    ETW_MAP_END

    ETW_VALUEMAP(PsRequestStatus)
        Pending   = 1,
        Active    = 2,
        Blocked   = 3,
        Complete  = 4,
        Abandoned = 5
    ETW_MAP_END

    ETW_TASK_BEGIN(Gui)
        ETW_PUSH_KEYWORDS(MessageHook)

            ETW_EVENT_I(WmGeneric, message="%1 %2(%3, %4)")(
                ETW_OUT(HexInt32) uint32_t hwnd,
                                       WMs message,
                ETW_OUT(HexInt32) uint32_t wParam,
                ETW_OUT(HexInt32) uint32_t lParam);

            ETW_EVENT_I(WmCommand, message="%1 %2(%3, ctrlId=%4, tgtHwnd=%5)")(
                ETW_OUT(HexInt32) uint32_t hwnd,
                                       WMs message,
                ETW_OUT(HexInt32) uint32_t wParam,
                ETW_OUT(HexInt32) uint32_t ctrlId,
                ETW_OUT(HexInt32) uint32_t targetHwnd);

            ETW_EVENT_I(WmCreate, keywords="WmCreate", message="%1 %2([%3], name=[%4], style=%5, ex=%6, (%7, %8, %9, %10))")(
                ETW_OUT(HexInt32) uint32_t hwnd,
                                       WMs message,
                                   LPCWSTR winclass,
                                   LPCWSTR name,
                ETW_OUT(HexInt32) uint32_t style,
                ETW_OUT(HexInt32) uint32_t exStyle,
                int32_t x, int32_t y, int32_t w, int32_t h);

            ETW_EVENT_I(WmPaint,      message="%1 %2(%3, %4, %5, %6)")            (ETW_OUT(HexInt32) uint32_t hwnd, WMs message, int32_t left, int32_t top, int32_t right, int32_t bottom);
            ETW_EVENT_I(WmKeyboard,   message="%1 %2(%3, %4)")                    (ETW_OUT(HexInt32) uint32_t hwnd, WMs message, VKs vk, ETW_OUT(HexInt32) uint32_t lParam);
            ETW_EVENT_I(WmMouse,      message="%1 %2((%5, %6), hit=%3, extra=%4)")(ETW_OUT(HexInt32) uint32_t hwnd, WMs message, HitTest hitTest, ETW_OUT(HexInt32) uint32_t extraInfo, int32_t x, int32_t y);
            ETW_EVENT_I(WmSysCommand, message="%1 %2(cmd=%3, (%4, %5))")          (ETW_OUT(HexInt32) uint32_t hwnd, WMs message, WmSysCommand command, int32_t x, int32_t y);
            ETW_EVENT_I(WmSetText,    message="%1 %2(%3)")                        (ETW_OUT(HexInt32) uint32_t hwnd, WMs message, LPCWSTR text);
        ETW_POP_KEYWORDS
    ETW_TASK_END

    ETW_TASK_BEGIN(GuiDump)
        ETW_EVENT_I(GuiDumpEvt, keywords="GuiDump GuiDumpGetText", message="GUI(%1, gen=%2, depth=%3, %9, dlgId=%6, class=[%7], text=[%8])")(
            ETW_OUT(HexInt32) uint32_t hwnd,
            uint16_t generation,
            uint16_t depth,
            uint32_t pid,
            uint32_t tid,
            uint32_t dlgId,
            LPCWSTR wndClass,
            LPCWSTR wndText,
            LPCSTR rect);
    ETW_TASK_END

    ETW_TASK_BEGIN(Logging)
        ETW_PUSH_KEYWORDS(Logging)
            ETW_EVENT_I(Log,      message="Log([%1])")                           (LPCSTR msg);
            ETW_EVENT_I(Log2,     message="Log([%1], [%2], %3, %4)")             (LPCSTR msg, LPCSTR str, int32_t param1, int32_t param2);
            ETW_EVENT_I(LogTable, message="LogTable(%1, [%2], empId=%3)")        (LogTableType type, LPCSTR msg, uint32_t empId);
            ETW_EVENT_I(LogInt,   message="LogInt(%2, [%1], %3)")                (LPCSTR msg, Category type, int32_t param1);
            ETW_EVENT_I(LogInt2,  message="LogInt(%2, [%1], %3, %4)")            (LPCSTR msg, Category type, int32_t param1, int32_t param2);
            ETW_EVENT_I(LogInt3,  message="LogInt(%2, [%1], %3, %4, %5)")        (LPCSTR msg, Category type, int32_t param1, int32_t param2, int32_t param3);
            ETW_EVENT_I(LogInt4,  message="LogInt(%2, [%1], %3, %4, %5, %6)")    (LPCSTR msg, Category type, int32_t param1, int32_t param2, int32_t param3, int32_t param4);
            ETW_EVENT_I(LogInt5,  message="LogInt(%2, [%1], %3, %4, %5, %6, %7)")(LPCSTR msg, Category type, int32_t param1, int32_t param2, int32_t param3, int32_t param4, int32_t param5);

            ETW_EVENT_V(TraceVerbose,  message="%3 Verbose [%1] %4")(LPCWSTR category, LPCWSTR method, int32_t indent, LPCWSTR message);
            ETW_EVENT_I(TraceInfo,     message="%3    Info [%1] %4")(LPCWSTR category, LPCWSTR method, int32_t indent, LPCWSTR message);
            ETW_EVENT_W(TraceWarn,     message="%3 Warning [%1] %4")(LPCWSTR category, LPCWSTR method, int32_t indent, LPCWSTR message);
            ETW_EVENT_E(TraceError,    message="%3   Error [%1] %4")(LPCWSTR category, LPCWSTR method, int32_t indent, LPCWSTR message);
            ETW_EVENT_C(TraceCritical, message="%3  !Crit! [%1] %4")(LPCWSTR category, LPCWSTR method, int32_t indent, LPCWSTR message);

            ETW_EVENT_V(SetPosActive,                                 message="SetPosActive(New:%1, Old:%2, Count:%3)")(bool newStatus, bool oldStatus, int32_t count);
            ETW_EVENT_V(TraceVerboseStart, opcode="win:Start",        message="%3 Verbose [%1] %4")                    (LPCWSTR category, LPCWSTR method, int32_t indent, LPCWSTR message);
            ETW_EVENT_V(TraceVerboseStop,  opcode="win:Stop",         message="%3 Verbose [%1] %4 (%5 ms)")            (LPCWSTR category, LPCWSTR method, int32_t indent, LPCWSTR message, float intervalMs);
            ETW_EVENT_C(ProcessExit,       keywords="MustTraceStack", message="ProcessExit(%1, %2)")                   (LPCSTR method, LPCSTR msg);
        ETW_POP_KEYWORDS
    ETW_TASK_END

    ETW_TASK_BEGIN(TimedLogging)
        ETW_PUSH_KEYWORDS(Logging)
            ETW_EVENT_I(LogTimedStart,   opcode="win:Start", message="LogTimedStart(%2, %3, [%1])")                (LPCSTR msg, Category type, int32_t correlation);
            ETW_EVENT_I(LogTimedFinish,  opcode="win:Stop",  message="LogTimedFinish(%2, %3, [%1], ms=%4)")        (LPCSTR msg, Category type, int32_t correlation, float intervalMs);
            ETW_EVENT_I(LogTimedStart2,  opcode="win:Start", message="LogTimedStart(%2, %3, [%1], %4)")            (LPCSTR msg, Category type, int32_t correlation, int32_t param1);
            ETW_EVENT_I(LogTimedFinish2, opcode="win:Stop",  message="LogTimedFinish(%2, %3, [%1], %5, ms=%4)")    (LPCSTR msg, Category type, int32_t correlation, float intervalMs, int32_t param1);
            ETW_EVENT_I(LogTimedStart3,  opcode="win:Start", message="LogTimedStart(%2, %3, [%1], %4, %5)")        (LPCSTR msg, Category type, int32_t correlation, int32_t param1, int32_t param2);
            ETW_EVENT_I(LogTimedFinish3, opcode="win:Stop",  message="LogTimedFinish(%2, %3, [%1], %5, %6, ms=%4)")(LPCSTR msg, Category type, int32_t correlation, float intervalMs, int32_t param1, int32_t param2);
            ETW_EVENT_I(MethodBegin,     opcode="win:Start", message="Begin(%1, %2, %3, %4, %5, %6, %7, %8)")      (LPCWSTR Category, LPCWSTR MethodName, LPCWSTR p1, LPCWSTR p2, LPCWSTR p3, LPCWSTR p4, LPCWSTR p5, LPCWSTR pextra);
            ETW_EVENT_I(MethodEnd,       opcode="win:Stop",  message="End(%1, %2) - %3 ms")                        (LPCWSTR Category, LPCWSTR MethodName, int64_t MillisecondsElapsed);
        ETW_POP_KEYWORDS
    ETW_TASK_END

    ETW_TASK_BEGIN(EmitJson)
        ETW_EVENT_I(Json, keywords="Json", message="JSON(%1): %2")(LPCSTR reason, LPCSTR json);
    ETW_TASK_END

    ETW_TASK_BEGIN(Errors)
        ETW_EVENT_E(ComError, keywords="MustTraceStack", message="ComError(%1, [%2]")(
            ETW_OUT(HResult) int32_t hr,
            LPCSTR description);

        ETW_EVENT_E(CaughtException, message="CaughtException(%1, [%2], %3, %4)")(
            bool isUserVisible,
            LPCWSTR description,
            ETW_OUT(HResult) int32_t hr,
            ETW_OUT(HexInt64) uint64_t stackPointer);

        ETW_EVENT_W(RaisedException, keywords="MustTraceStack", message="RaisedException(%1, %2, %3)")(
            LPCSTR type,
            LPCWSTR message,
            ETW_OUT(HResult) int32_t result);

    ETW_TASK_END

    ETW_TASK_BEGIN(Network)
        ETW_PUSH_KEYWORDS(NetworkTrace)
            ETW_EVENT_V(LegacySend,             message="LegacySend(%1, %2)")(LPCSTR type, LPCSTR json);
            ETW_EVENT_V(LegacyRecv,             message="LegacyRecv(%1, %2)")(LPCSTR type, LPCSTR json);
            ETW_EVENT_V(PersistenceEventRemote, message="RemotePersist(%1, %2, %3, %4)")(PersistenceEventType Event, LPCWSTR Type, int32_t Identity, uint32_t UpdatedPropertyCount);
            ETW_EVENT_V(PersistenceEventLocal,  message="LocalPersist(%1, %2, %3, %4)")(PersistenceEventType Event, LPCWSTR Type, int32_t Identity, uint32_t UpdatedPropertyCount);
            ETW_EVENT_V(PersistenceClient,      message="PersistenceClient(%1, %2)")(PersistenceClientEvent Event, int32_t MessageNumber);
        ETW_POP_KEYWORDS
    ETW_TASK_END

    ETW_TASK_BEGIN(PaymentServer)
        ETW_PUSH_KEYWORDS(Payments)
            ETW_EVENT_I(PsBeginRequest, opcode="win:Start", message="PsBeginRequest(%1, %2)")         (int32_t requestId, LPCWSTR message, int32_t paymentId, const GUID& sessionId);
            ETW_EVENT_I(PsEndRequest,   opcode="win:Stop" , message="PsEndRequest(%1, %2)")           (int32_t requestId, LPCWSTR message, int32_t paymentId, const GUID& sessionId);
            ETW_EVENT_I(PsLog,                              message="PsLog(%1, %2, %3, %4)")          (int32_t requestId, LPCWSTR message, int32_t paymentId, const GUID& sessionId);
            ETW_EVENT_E(PsError,                            message="PsError(%1, %2, %3, %4, %5, %6)")(int32_t requestId, LPCWSTR message, int32_t paymentId, const GUID& sessionId, LPCWSTR stack, int32_t depth);

            ETW_EVENT_V(PsDumpRequest, message="PsRequest(Gen:%1, R:%2, S:%3, %4 - %5, TID:%6:%7, %8 - %9)")(
                    int32_t generation,
                    int32_t requestId,
                    const GUID& sessionId,
                    LPCWSTR type,
                    PsRequestStatus status,
                    uint16_t threadId,
                    ETW_OUT(HexInt16) uint16_t threadState,
                    const FILETIME& startTimeUtc,
                    const FILETIME& finishTimeUtc);
                    
        ETW_POP_KEYWORDS
    ETW_TASK_END

    ETW_TASK_BEGIN(Locking)
        ETW_PUSH_KEYWORDS(Misc)
            ETW_EVENT_V(AcquireLock,      message="AcquireLock(%1, %2, %3, newTid=%4, newSpid=%5, oldTid=%6, oldSpid=%7)") (LPCSTR Type, LPCSTR Key, LockStatus Status, int32_t NewTerminal, int16_t NewSPID, int32_t OldTerminal, int16_t OldSPID);
            ETW_EVENT_V(ReleaseLock,      message="ReleaseLock(%1, %2, %3, newTid=%4, newSpid=%5, oldTid=%6, oldSpid=%7)") (LPCSTR Type, LPCSTR Key, LockStatus Status, int32_t NewTerminal, int16_t NewSPID, int32_t OldTerminal, int16_t OldSPID);
            ETW_EVENT_V(AcquireVisitLock, message="AcquireVisitLock(%1, %2, newTid=%3, newSpid=%4, oldTid=%5, oldSpid=%6)")(int32_t VisitId, LockStatus Status, int32_t NewTerminal, int16_t NewSPID, int32_t OldTerminal, int16_t OldSPID);
            ETW_EVENT_V(ReleaseVisitLock, message="ReleaseVisitLock(%1, %2, newTid=%3, newSpid=%4, oldTid=%5, oldSpid=%6)")(int32_t VisitId, LockStatus Status, int32_t NewTerminal, int16_t NewSPID, int32_t OldTerminal, int16_t OldSPID);
        ETW_POP_KEYWORDS
    ETW_TASK_END

    ETW_TASK_BEGIN(ImageCache)
        ETW_EVENT_E(ImageCacheError, message="ImageCacheError(%1, %2, [%3])")(ImageSource source, int32_t id, LPCSTR message);
        ETW_EVENT_I(ImageCacheLoad, message="ImageCacheLoad(%1, %2, %3, %4, %5)")(
            ImageSource source,
            int32_t id,
            bool isNewHash,
            ETW_OUT(HexBinary, length=16) ETW_IN(EtwInType::Binary) const char* hash,
            uint32_t imageSize);
    ETW_TASK_END

    ETW_TASK_BEGIN(DomainTrace)
        ETW_PUSH_KEYWORDS(DomainTrace)
            ETW_EVENT_E(DTError, keywords="MustTraceStack", message="Error(%1, %2, %3, %4)")(LPCSTR category, LPCSTR message, int32_t param1, LPCWSTR param2);
            ETW_EVENT_W(DTWarning, message="Warning(%1, %2, %3, %4)")(LPCSTR category, LPCSTR message, int32_t param1, LPCWSTR param2);

            ETW_EVENT_V(DTUnitOfWorkBegin,       message="UnitOfWorkBegin(%1, %2)")      (int32_t unitId, int32_t depth, LPCWSTR typeName, uint32_t objectId);
            ETW_EVENT_V(DTUnitOfWorkCommitStart, message="UnitOfWorkCommitStart(%1, %2)")(int32_t unitId, int32_t depth, LPCWSTR typeName, uint32_t objectId);
            ETW_EVENT_V(DTUnitOfWorkCommitStop,  message="UnitOfWorkCommitStop(%1, %2)") (int32_t unitId, int32_t depth, LPCWSTR typeName, uint32_t objectId);
            ETW_EVENT_V(DTUnitOfWorkAbortStart,  message="UnitOfWorkAbortStart(%1, %2)") (int32_t unitId, int32_t depth, LPCWSTR typeName, uint32_t objectId);
            ETW_EVENT_V(DTUnitOfWorkAbortStop,   message="UnitOfWorkAbortStop(%1, %2)")  (int32_t unitId, int32_t depth, LPCWSTR typeName, uint32_t objectId);
            ETW_EVENT_V(DTUnitOfWorkInsert,      message="UnitOfWorkInsert(%1, %3, %4)") (int32_t unitId, int32_t depth, LPCWSTR typeName, uint32_t objectId);
            ETW_EVENT_V(DTUnitOfWorkUpdate,      message="UnitOfWorkUpdate(%1, %3, %4)") (int32_t unitId, int32_t depth, LPCWSTR typeName, uint32_t objectId);
            ETW_EVENT_V(DTUnitOfWorkDelete,      message="UnitOfWorkDelete(%1, %3, %4)") (int32_t unitId, int32_t depth, LPCWSTR typeName, uint32_t objectId);
        ETW_POP_KEYWORDS
    ETW_TASK_END

    ETW_VALUEMAP(BdeMethod)
        AppendRecord      = 0,
        BeginTran         = 1,
        DeleteRecord      = 2,
        EndTran           = 3,
        ForceRecordReread = 4,
        ForceReread       = 5,
        GetNextRecord     = 6,
        GetPriorRecord    = 7,
        GetRecord         = 8,
        GetRecordForKey   = 9,
        InsertRecord      = 10,
        ModifyRecord      = 11,
        QExecDirect       = 12,
        SaveChanges       = 13
    ETW_MAP_END

    ETW_TASK_BEGIN(Statistics)
        ETW_EVENT_V(BdeStats, keywords="Statistics", message="BdeStats: %1 delta = %2, total = %3")(BdeMethod method, int32_t delta, int32_t total);
    ETW_TASK_END

    ETW_TASK_BEGIN(LogicalOp)
        ETW_PUSH_KEYWORDS(Logging)
            ETW_EVENT_I(LogicalOpStart, opcode="win:Start", message="LogicalOpStart(%1, %2, %3, %4, %5)")   (int32_t correlation, LPCSTR msg, LPCSTR param1, int32_t param2, int32_t param3);
            ETW_EVENT_I(LogicalOpStop,  opcode="win:Stop",  message="LogicalOpStop(%1, %2, %3, %4, %5, %6)")(int32_t correlation, LPCSTR msg, LPCSTR param1, int32_t param2, int32_t param3, float intervalMs);
        ETW_POP_KEYWORDS
    ETW_TASK_END

ETW_PROVIDER_END

class TracingIndentationManager
{
	DWORD s_tlsSlot;

	TracingIndentationManager(const TracingIndentationManager &) {}
	TracingIndentationManager &operator=(TracingIndentationManager const& other) {}
public:
	TracingIndentationManager() : s_tlsSlot(TlsAlloc()) { }
	~TracingIndentationManager() { TlsFree(s_tlsSlot); }

	DWORD GetIndent()
	{
		return (DWORD)TlsGetValue(s_tlsSlot);
	}

	void Indent()
	{
		TlsSetValue(s_tlsSlot, (LPVOID)(GetIndent()+1));
	}

	void Unindent()
	{
		DWORD indent = GetIndent();
		if (indent > 0)
			TlsSetValue(s_tlsSlot, (LPVOID)(indent - 1));
	}
};

extern TracingIndentationManager g_IndentationManager;

class SHORTCUTSETL_API ShortcutsTracer
{
	LPCWSTR m_sourceType;
	LPCWSTR m_category;
	LPCWSTR m_shortCategory;

	ShortcutsTracer() {}
public:
	static const int BufferSize = 256;
	ShortcutsTracer(LPCWSTR sourceType, LPCWSTR category, LPCWSTR shortCategory)
		: m_sourceType(sourceType),
		m_category(category),
		m_shortCategory(shortCategory)
	{ }

	void __cdecl Verbose(LPCWSTR methodName, LPCWSTR format, ...);
	void __cdecl VerboseStart(LPCWSTR methodName, LPCWSTR message); // Should only be used via ScopedLog
	void __cdecl VerboseStop(LPCWSTR methodName, LPCWSTR message, float intervalMs); // Should only be used with via ScopedLog
	void __cdecl Information(LPCWSTR methodName, LPCWSTR format, ...);
	void __cdecl Warning(LPCWSTR methodName, LPCWSTR format, ...);
	void __cdecl Error(LPCWSTR methodName, LPCWSTR format, ...);
	void __cdecl Critical(LPCWSTR methodName, LPCWSTR format, ...);
	void WriteTraceEvent(ScEtl::LogLevel severity, LPCWSTR methodName, LPCWSTR message);
};

class SHORTCUTSETL_API ScopedLog
{
	ShortcutsTracer &m_tracer;
	LPCWSTR m_methodName;
	WCHAR m_message[ShortcutsTracer::BufferSize];
	LONGLONG m_ticks;

public:
	ScopedLog(ShortcutsTracer &tracer, LPCWSTR methodName, LPCWSTR format, ...);
	~ScopedLog();
};

#endif // _SHORTCUTS_ETL_H_
