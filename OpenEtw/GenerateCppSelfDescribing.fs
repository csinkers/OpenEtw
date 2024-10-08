﻿module internal OpenEtw.GenerateCppSelfDescribing
open OpenEtw.Util

let buildCpp (provider : EtwProvider) (options : CppSelfDescribingOptions) =
    let manifest = provider |> (fun p -> (p.name, GenerateManifest.forProvider p))
    let escapeForCpp : (string -> string seq) = (fun s ->
        let splitIntoChunks maxChunkSize (s:string) = seq {
            for i in [0..(s.Length / maxChunkSize)] do
                yield s.Substring(i * maxChunkSize, System.Math.Min(maxChunkSize, s.Length - i * maxChunkSize))
        }

        splitIntoChunks ((1 <<< 16) - 8) s |> Seq.map (fun chunk ->
            let lines = splitIntoChunks 160 chunk |> Seq.map (fun line ->
                let sb = new System.Text.StringBuilder()
                for c in line do
                    match c with
                    | '"' -> sb.Append("\\\"")
                    | '\\' -> sb.Append("\\\\")
                    | '\n' -> sb.Append("\\n")
                    | '\r' -> sb.Append("\\r")
                    | '\t' -> sb.Append("\\t")
                    | '\b' -> sb.Append("\\b")
                    | _ -> sb.Append c
                    |> ignore
                sb.ToString())

            lines
            |> Seq.mapi (fun i l -> sprintf "    \"%s\"" l)
            |> String.concat System.Environment.NewLine
        ))

    let getId typeName collection selector key =
        let pickFunction x =
            let entityId, entityName = selector x
            if (entityName = key) then Some entityId else None

        let result = collection |> List.tryPick pickFunction
        match result with
        | Some x -> x
        | None -> failwithf "Error: %s \"%s\" was not defined." typeName key

    let getIdForChannel = getId "Channel" provider.channels (fun c -> (c.id, c.name))
    let getIdForLevel   = getId "Level"   provider.levels   (fun l -> (l.id, l.name))
    let getIdForOpcode  = getId "Opcode"  provider.opcodes  (fun o -> (o.id, o.name))
    let getIdForTask    = getId "Task"    provider.tasks    (fun t -> (t.id, t.name))

    let getMaskForKeywords =
        function
        | [] -> 0UL
        | names ->
            let keywords = names |> List.map (fun n -> provider.keywords |> List.find (fun k -> k.name = n))
            List.fold (|||) 0UL (keywords |> List.map (fun k -> k.id))

    let populateArgument i p =
        //let ex = match p.expression with
        //         | Some ex -> ex
        //         | None -> p.name

        match p.count with
        | EtwCount.Single ->
            match p.inType with
            | UnicodeString  NullTerminated -> sprintf "    ETW_UNICODE_PARAM(%d, %s)" i p.name
            | AnsiString     NullTerminated -> sprintf "    ETW_ANSI_PARAM(%d, %s)" i p.name
            | EtwType.Binary NullTerminated -> failwith "Binary parameters must have a length supplied"

            | EtwType.UnicodeString (EtwLength.Fixed n)
            | EtwType.AnsiString (EtwLength.Fixed n)
            | EtwType.Binary (EtwLength.Fixed n) ->
                sprintf "    ETW_COUNTED_PTR_PARAM(%s, %d, %s, %d)" (p.inType.CanonicalType()) i p.name n

            | EtwType.UnicodeString (EtwLength.Counted arg)
            | EtwType.AnsiString (EtwLength.Counted arg)
            | EtwType.Binary (EtwLength.Counted arg) ->
                sprintf "    ETW_COUNTED_PTR_PARAM(%s, %d, %s, %s)" (p.inType.CanonicalType()) i p.name arg

            | EtwType.Guid
            | EtwType.FileTime
            | EtwType.SystemTime -> sprintf "    ETW_PTR_PARAM(%s, %d, %s)" (p.inType.CanonicalType()) i p.name
            | EtwType.Unresolved s -> failwithf "Tried to emit unresolved type %s" s
            | _ -> sprintf "    ETW_LITERAL_PARAM(%s, %d, %s)" (p.inType.CanonicalType()) i p.name

        | EtwCount.Fixed count        ->
            failwith "TODO" // sprintf "    ETW_COUNTED_PTR_PARAM(%s, %d, %s, %d)" (p.inType.CanonicalType()) i ex count
        | EtwCount.Counted countParam ->
            failwith "TODO" // sprintf "    ETW_COUNTED_PTR_PARAM(%s, %d, %s, %s)" (p.inType.CanonicalType()) i ex countParam

    let perHeader f = provider.headers |> List.map f |> String.concat System.Environment.NewLine
    let perEvent f = provider.events |> List.map f |> String.concat System.Environment.NewLine
    let perTask f = provider.tasks |> List.map f |> String.concat System.Environment.NewLine
    let lookupTask name = provider.tasks |> List.find (fun t -> t.name = name)
    let groupedContexts = // ((contextId, keywordMask, level), eventId list) list
        provider.events
        |> List.map (fun e -> ((e.keywords |> getMaskForKeywords, e.level |> getIdForLevel), e.id))
        |> List.groupBy fst
        |> List.mapi (fun i ((k, l), members) -> ((i,k,l), members |> List.map snd))

    let eventContextIdLookup = // map from eventId to contextId
        groupedContexts
        |> List.collect (fun ((i,k,l), eventIds) -> eventIds |> List.map (fun e -> (e, i)) )
        |> Map.ofList

    let distinctContexts = groupedContexts |> List.map fst // (contextId:int, keywordMask:uint64, level:int) list
    let distinctContextCount = List.length distinctContexts
    let requiredBitMasks = (distinctContextCount + 31) / 32 // 32 bits in a ULONG

    [
        (perHeader (fun h -> if h.StartsWith "<" then sprintf "#include %s" h else sprintf "#include \"%s\"" h))
        (sprintf """// -----------------------------------------------------
//  %s
//  DO NOT EDIT BY HAND
// -----------------------------------------------------""" options.etwGenComment)

        (sprintf """
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#include <stdlib.h> // for _countof
#include <wmistr.h>
#include <evntrace.h>
#include <evntprov.h>
#include <strsafe.h>
#include "%s" """         options.headerName)

        (if options.insertDebugLogging then
            """#define ETW_DEBUG(x, ...) OutputDebugStringA(x);"""
//            """#define ETW_DEBUG(...) printf(__VA_ARGS__);"""
         else "#define ETW_DEBUG(...)")

        // Helper macros
        """#define ETW_ENABLED_CHECK(enableBit) if ((ProviderEnableBits[enableBit >> 5] & (1 << (enableBit % 32))) == 0) { return; }

#define ETW_ANSI_PARAM(n, p) const char *arg##n = (p);\
    ULONG arg##n##Len = (ULONG)(strlen(arg##n) + 1);\
    EventDataDescCreate(&eventData[n],\
            (arg##n != NULL) ? arg##n : "NULL",\
            (arg##n != NULL) ? (arg##n##Len > 64000 ? 64000 : arg##n##Len) : (ULONG)sizeof("NULL"));

#define ETW_UNICODE_PARAM(n, p) const wchar_t *arg##n = (p);\
    ULONG arg##n##Len = (ULONG)(wcslen(arg##n) + 1) * sizeof(wchar_t);\
    EventDataDescCreate(&eventData[n],\
            (arg##n != NULL) ? arg##n : L"NULL",\
            (arg##n != NULL) ? (arg##n##Len > 64000 ? 64000 : arg##n##Len) : (ULONG)sizeof("NULL"));

#define ETW_LITERAL_PARAM(type, n, p) type arg##n = (p);\
    EventDataDescCreate(&eventData[n], &arg##n, sizeof(arg##n));

#define ETW_PTR_PARAM(type, n, p) type arg##n = (p);\
    EventDataDescCreate(&eventData[n], arg##n, sizeof(*arg##n));

#define ETW_COUNTED_PTR_PARAM(type, n, p, l) type arg##n = (p);\
    EventDataDescCreate(&eventData[n], arg##n, (l) * sizeof(*arg##n));

// If running on SDK v6.1 these won't be available
#ifndef EVENT_CONTROL_CODE_DISABLE_PROVIDER
#define EVENT_CONTROL_CODE_DISABLE_PROVIDER 0
#define EVENT_CONTROL_CODE_ENABLE_PROVIDER  1
#define EVENT_CONTROL_CODE_CAPTURE_STATE    2
#endif
"""

        (sprintf """static const char *etwManifest[] = {
%s
};
"""         (escapeForCpp (snd manifest) |> String.concat ("," + System.Environment.NewLine)))

        (sprintf "// %s = %s%s" (provider.guid.ToString()) (if Util.providerToGuid provider.symbol = provider.guid then "*" else "") provider.symbol)
        (sprintf "const GUID ProviderGuid = %s;" (provider.guid.ToString("X")))

        (sprintf """
static const int EnableBitsCount = %d;
static REGHANDLE                 ProviderHandle = (REGHANDLE)0;
static DECLSPEC_CACHEALIGN ULONG ProviderEnableBits[%d];
static const ULONGLONG           ProviderKeywords[EnableBitsCount] = {%s};
static const UCHAR               ProviderLevels[EnableBitsCount] = {%s};"""
            distinctContextCount
            requiredBitMasks
            (distinctContexts |> List.map (fun (_, k, _) -> sprintf "0x%x" k) |> String.concat ", ")
            (distinctContexts |> List.map (fun (_, _, l) -> sprintf "0x%x" l) |> String.concat ", "))

        """
                                                //  Id      Vers Chan Lvl  Opcode Task   Keyword
static const EVENT_DESCRIPTOR Event_EmitManifest = {0xfffe, 0x1, 0x0, 0x0, 0xfe, 0xfffe, 0x00FFFFFFFFFFFFFF};
static void EmitManifest()
{
    if (!ProviderHandle)
    {
        ETW_DEBUG("ETW_DEBUG: Skipping manifest emission due to uninitialised provider handle\n");
        return;
    }

    unsigned char format        = 1;
    unsigned char majorVersion  = 1;
    unsigned char minorVersion  = 0;
    unsigned char magicNumber   = 0x5b;
    unsigned short totalChunks  = (unsigned short)_countof(etwManifest);

    for (unsigned short i = 0; i < totalChunks; i++)
    {
        EVENT_DATA_DESCRIPTOR eventData[7];
        EventDataDescCreate(&eventData[0], &format,        sizeof(format));
        EventDataDescCreate(&eventData[1], &majorVersion,  sizeof(majorVersion));
        EventDataDescCreate(&eventData[2], &minorVersion,  sizeof(minorVersion));
        EventDataDescCreate(&eventData[3], &magicNumber,   sizeof(magicNumber));
        EventDataDescCreate(&eventData[4], &totalChunks,   sizeof(totalChunks));
        EventDataDescCreate(&eventData[5], &i,             sizeof(i));
        EventDataDescCreate(&eventData[6], etwManifest[i], (ULONG)strlen(etwManifest[i]));"""
        sprintf """        ETW_DEBUG("ETW_DEBUG: Emitting manifest chunk %%d of %%d for %s\n", i+1, totalChunks);""" provider.symbol
        """        EventWrite(ProviderHandle, &Event_EmitManifest, _countof(eventData), eventData);
    }
}

static void __stdcall ControlCallbackV2(LPCGUID, ULONG controlCode, UCHAR level, ULONGLONG matchAnyKeyword, ULONGLONG matchAllKeyword, PEVENT_FILTER_DESCRIPTOR, PVOID)
{
    switch (controlCode)
    {
        case EVENT_CONTROL_CODE_ENABLE_PROVIDER:
            ETW_DEBUG("ETW_DEBUG: Provider enabled\n");
            for (ULONG ix = 0; ix < EnableBitsCount; ++ix)
            {
                bool levelMatched = (ProviderLevels[ix] <= level) || (level == 0);
                bool keywordMatched =
                    ((ProviderKeywords[ix] == (ULONGLONG)0) ||
                    ((ProviderKeywords[ix] & matchAnyKeyword) && ((ProviderKeywords[ix] & matchAllKeyword) == matchAllKeyword)));

                if (levelMatched && keywordMatched)
                {
                    ProviderEnableBits[ix >> 5] |= (1 << (ix % 32));
                }
                else
                {
                    ProviderEnableBits[ix >> 5] &= ~(1 << (ix % 32));
                }
            }
            EmitManifest();
            break;

        case EVENT_CONTROL_CODE_DISABLE_PROVIDER:
            ETW_DEBUG("ETW_DEBUG: Provider disabled\n");
            EmitManifest();
            if (EnableBitsCount > 0)
            {
                RtlZeroMemory(ProviderEnableBits, (EnableBitsCount + 31)/32 * sizeof(ULONG));
            }
            break;

        default:
            break;
    }
}"""

        (sprintf """
ULONG %s::Register() // This function registers the provider with ETW.
{
    if (ProviderHandle) // already registered
        return ERROR_SUCCESS;

    ETW_DEBUG("ETW_DEBUG: Registering provider\n");
    const ULONG result = EventRegister(&ProviderGuid, ControlCallbackV2, NULL, &ProviderHandle);
    if (ProviderHandle)
        EmitManifest();

    return result;
}

ULONG %s::Unregister() // Unregister the provider
{
    if (!ProviderHandle) // Provider has not been registered
        return ERROR_SUCCESS;

    ETW_DEBUG("ETW_DEBUG: Deregistering provider\n");
    EmitManifest();
    ULONG error = EventUnregister(ProviderHandle);
    ProviderHandle = (REGHANDLE)0;

    return error;
}
""" provider.className provider.className)

        "// Events"

        (perEvent (fun e ->
            let interfaceParam p = sprintf "%s %s" p.cppType p.name

            [
                (sprintf "const EVENT_DESCRIPTOR Event_%s = {0x%x, 0x%x, 0x%x, 0x%x, 0x%x, 0x%x, 0x%x}; // IVCLOTK"
                    e.symbol
                    e.id.Value
                    e.version
                    ((e.channel  |> Option.map getIdForChannel |?? Some 0uy).Value)
                    (e.level    |> getIdForLevel)
                    (e.opcode   |> Option.map getIdForOpcode  |?? 0uy)
                    (e.task     |> Option.map getIdForTask    |?? 0us)
                    (e.keywords |> getMaskForKeywords))

                (
                let templateParameters = e.parameters |> List.where EtwEventParam.isTemplateParameter
                let activityId = e.parameters |> List.tryFind (fun p -> match p.inType with |EtwType.ActivityId -> true |_ -> false)
                let relatedActivityId = e.parameters |> List.tryFind (fun p -> match p.inType with |EtwType.RelatedActivityId -> true |_ -> false)

                match e.parameters, activityId, relatedActivityId with
                | [], None, None ->
                    sprintf """void %s::%s()
{
    ETW_ENABLED_CHECK(%d)
    ETW_DEBUG("ETW_DEBUG: Emitting event '%s'\n");
    EventWrite(ProviderHandle, &Event_%s, 0, NULL);
}
"""                     provider.className
//                        provider.prefix
                        e.cppName
                        (eventContextIdLookup |> Map.find e.id)
//                        (match e.supplementaryCode with |Some x -> x.Replace("\n", System.Environment.NewLine) |None -> "")
                        e.cppName
                        e.symbol
                | [], _, _ ->
                    sprintf """void %s::%s()
{
    ETW_ENABLED_CHECK(%d)
    ETW_DEBUG("ETW_DEBUG: Emitting event '%s'\n");
    EventWriteEx(ProviderHandle, &Event_%s, 0, 0, %s, %s, 0, NULL);
}
"""                     provider.className
//                        provider.prefix
                        e.cppName
                        (eventContextIdLookup |> Map.find e.id)
//                        (match e.supplementaryCode with |Some x -> x.Replace("\n", System.Environment.NewLine) |None -> "")
                        e.cppName
                        e.symbol
                        (activityId |> Option.map (fun p -> p.name) |?? "NULL")
                        (relatedActivityId |> Option.map (fun p -> p.name) |?? "NULL")

                | _, None, None ->
                    sprintf """void %s::%s(%s)
{
    ETW_ENABLED_CHECK(%d)
    ETW_DEBUG("ETW_DEBUG: Emitting event '%s'\n");
    EVENT_DATA_DESCRIPTOR eventData[%d];
%s
    EventWrite(ProviderHandle, &Event_%s, _countof(eventData), eventData);
}
"""                     provider.className
//                        provider.prefix
                        e.cppName
                        (e.parameters |> Seq.map interfaceParam |> String.concat ", ")
                        (eventContextIdLookup |> Map.find e.id)
//                        (match e.supplementaryCode with |Some x -> x.Replace("\n", System.Environment.NewLine) |None -> "")
                        e.cppName
                        templateParameters.Length
                        (templateParameters |> Seq.mapi populateArgument |> String.concat System.Environment.NewLine)
                        e.symbol
                | _ ->
                    sprintf """void %s::%s(%s)
{
    ETW_ENABLED_CHECK(%d)
    ETW_DEBUG("ETW_DEBUG: Emitting event '%s'\n");
    EVENT_DATA_DESCRIPTOR eventData[%d];
%s
    EventWriteEx(ProviderHandle, &Event_%s, 0, 0, %s, %s, _countof(eventData), eventData);
}
"""                     provider.className
//                        provider.prefix
                        e.cppName
                        (e.parameters |> Seq.map interfaceParam |> String.concat ", ")
                        (eventContextIdLookup |> Map.find e.id)
//                        (match e.supplementaryCode with |Some x -> x.Replace("\n", System.Environment.NewLine) |None -> "")
                        e.cppName
                        templateParameters.Length
                        (templateParameters |> Seq.mapi populateArgument |> String.concat System.Environment.NewLine)
                        e.symbol
                        (activityId |> Option.map (fun p -> p.name) |?? "NULL")
                        (relatedActivityId |> Option.map (fun p -> p.name) |?? "NULL"))

            ] |> String.concat System.Environment.NewLine
        ))

        (sprintf """class ProviderCleanup
{
public:
    ~ProviderCleanup() { %s::Unregister(); }
};

static ProviderCleanup g_providerCleanup;
"""           provider.className)
    ] |> String.concat System.Environment.NewLine

let forProvider (provider : EtwProvider) (options : CppSelfDescribingOptions) =
    [ (options.cppFilename, buildCpp provider options) ]
