namespace OpenEtw
open System

[<Measure>] type AbsoluteEtwTicks
[<Measure>] type EtwTicksAfterBoot

type ExtendedData =
    | RelatedActivityId of byte array
    | Sid               of byte array
    | TsId              of byte array
    | InstanceInfo      of byte array
    | StackTrace32      of (uint64 * uint32 list) // MatchId & addresses
    | StackTrace64      of (uint64 * uint64 list) // MatchId & addresses
    | PebsIndex         of byte array
    | PmcCounter        of byte array
    | PsmKey            of byte array
    | EventKey          of byte array
    | EventSchemaTl     of byte array
    | ProvTraits        of byte array
    | ProcessStartKey   of byte array

    static member Deserialize (w:Util.ISerializer) =
        let fullSize  = Util.readValue w.UInt16 "fullSize"  0us
        let dataType  = Util.readValue w.UInt16 "dataType"  0us
        let reserved2 = Util.readValue w.UInt16 "reserved2" 0us // Does this indicate if there's another extended data item following this one?
        let size = int (Util.readValue w.UInt16 "size"      0us)

        if (int fullSize <> size + 8) then 
            failwith "Reserved1 was expected to be size + 8 when decoding extended event data"

        let mutable value = RelatedActivityId [||]
        let blank = (fun () -> [||])
        let setter x = (fun v -> value <- x v)
        let arrayField typeConstructor = w.ByteArray "" (fun () -> [||]) (fun v -> value <- typeConstructor v) size

        match dataType with
        | 0x1us -> arrayField RelatedActivityId
        | 0x2us -> arrayField Sid
        | 0x3us -> arrayField TsId
        | 0x4us -> arrayField InstanceInfo
        | 0x5us -> 
            let matchId = Util.readValue w.UInt64 "matchId" 0UL
            let addresses = 
                List.ofSeq <| seq { 
                    for _ in [8..4..size-1] do 
                        yield (Util.readValue w.UInt32 "address" 0u) 
                }
            value <- StackTrace32 (matchId, addresses)

        | 0x6us -> 
            let matchId = Util.readValue w.UInt64 "matchId" 0UL
            let addresses = 
                List.ofSeq <| seq { 
                    for _ in [8..8..size-1] do 
                        yield (Util.readValue w.UInt64 "address" 0UL) 
                }
            value <- StackTrace64 (matchId, addresses)

        | 0x7us -> arrayField PebsIndex
        | 0x8us -> arrayField PmcCounter
        | 0x9us -> arrayField PsmKey
        | 0xaus -> arrayField EventKey
        | 0xbus -> arrayField EventSchemaTl
        | 0xcus -> arrayField ProvTraits
        | 0xdus -> arrayField ProcessStartKey
        | _ -> failwith "Unknown extended data type encountered"

        if(size % 8 <> 0) then
            w.RepeatU8 "" 0uy (Util.paddingBytes size)

        [value] // TODO: Multiple extended data items

    member x.Serialize (w:Util.ISerializer) =
        let constant x = (fun () -> x)

        let enumValue, name, size =
            match x with
            | RelatedActivityId p -> (0x1us, "RelatedActivityId", p.Length)
            | Sid p               -> (0x2us, "Sid", p.Length)
            | TsId p              -> (0x3us, "TsId", p.Length)
            | InstanceInfo p      -> (0x4us, "InstanceInfo", p.Length)
            | StackTrace32 (matchId, addresses) -> (0x5us, "StackTrace32", 8 + addresses.Length * 4)
            | StackTrace64 (matchId, addresses) -> (0x6us, "StackTrace64", 8 + addresses.Length * 8)
            | PebsIndex p         -> (0x7us, "PebsIndex", p.Length)
            | PmcCounter p        -> (0x8us, "PmcCounter", p.Length)
            | PsmKey p            -> (0x9us, "PsmKey", p.Length)
            | EventKey p          -> (0xaus, "EventKey", p.Length)
            | EventSchemaTl p     -> (0xbus, "EventSchemaTl", p.Length)
            | ProvTraits p        -> (0xcus, "ProvTraits", p.Length)
            | ProcessStartKey p   -> (0xdus, "ProcessStartKey", p.Length)

        if(size > (int System.UInt16.MaxValue) || size < 8) then failwith "Invalid extended data size"

        w.Comment name
        Util.writeValue w.UInt16 "reserved1" (uint16 size + 8us)
        Util.writeValue w.UInt16 "type"      enumValue
        Util.writeValue w.UInt16 "size"      (uint16 size)
        Util.writeValue w.UInt16 "reserved2" 0us

        let arrayField payload = w.ByteArray "" (fun () -> payload) ignore size

        match x with
        | RelatedActivityId p -> arrayField p
        | Sid p               -> arrayField p
        | TsId p              -> arrayField p
        | InstanceInfo p      -> arrayField p
        | StackTrace32 (batchId, addresses) ->
            Util.writeValue w.UInt64 "batchId" batchId
            for address in addresses do Util.writeValue w.UInt32 "address" address
        | StackTrace64 (batchId, addresses) ->
            Util.writeValue w.UInt64 "batchId" batchId
            for address in addresses do Util.writeValue w.UInt64 "address" address
        | PebsIndex p         -> arrayField p
        | PmcCounter p        -> arrayField p
        | PsmKey p            -> arrayField p
        | EventKey p          -> arrayField p
        | EventSchemaTl p     -> arrayField p
        | ProvTraits p        -> arrayField p
        | ProcessStartKey p   -> arrayField p

        let paddingBytes = Util.paddingBytes size
        if (paddingBytes > 0) then
            w.RepeatU8 "padding" 0uy paddingBytes

[<Flags>]
type EventFlag =
    | None           = 0x0000us
    | ExtendedInfo   = 0x0001us
    | PrivateSession = 0x0002us
    | StringOnly     = 0x0004us
    | TraceMessage   = 0x0008us
    | NoCpuTime      = 0x0010us
    | Header32Bit    = 0x0020us
    | Header64Bit    = 0x0040us
    | ClassicHeader  = 0x0100us
    | ProcessorIndex = 0x0200us

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module EventFlag =
    let private valueLookup =
        [
            (EventFlag.ExtendedInfo   , "ExtendedInfo")
            (EventFlag.PrivateSession , "PrivateSession")
            (EventFlag.StringOnly     , "StringOnly")
            (EventFlag.TraceMessage   , "TraceMessage")
            (EventFlag.NoCpuTime      , "NoCpuTime")
            (EventFlag.Header32Bit    , "Header32Bit")
            (EventFlag.Header64Bit    , "Header64Bit")
            (EventFlag.ClassicHeader  , "ClassicHeader")
            (EventFlag.ProcessorIndex , "ProcessorIndex")
            (LanguagePrimitives.EnumOfValue 0x400us , "Unknown (0x400)")
            (LanguagePrimitives.EnumOfValue 0x800us , "Unknown (0x800)")
            (LanguagePrimitives.EnumOfValue 0x1000us, "Unknown (0x1000)")
            (LanguagePrimitives.EnumOfValue 0x2000us, "Unknown (0x2000)")
            (LanguagePrimitives.EnumOfValue 0x4000us, "Unknown (0x4000)")
            (LanguagePrimitives.EnumOfValue 0x8000us, "Unknown (0x8000)")
        ]

    let fromUInt16 = LanguagePrimitives.EnumOfValue
    let info (x : EventFlag) =
            let str = 
                if x = EventFlag.None then "None" else
                valueLookup
                |> Seq.choose (fun (v, name) -> if (x &&& v <> EventFlag.None) then Some name else None)
                |> String.concat " | "
            (uint16 x, str)

type EventLevel =
    | Always
    | Critical
    | Error
    | Warning
    | Informational
    | Verbose
    | Custom of byte * string

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module EventLevel =
    let info = 
        function
        | Always        -> 0uy, "Always"
        | Critical      -> 1uy, "Critical"
        | Error         -> 2uy, "Error"
        | Warning       -> 3uy, "Warning"
        | Informational -> 4uy, "Informational"
        | Verbose       -> 5uy, "Verbose"
        | Custom (n, s) -> n, s

    let fromUInt8 = 
        function
        | 1uy -> Critical      
        | 2uy -> Error         
        | 3uy -> Warning       
        | 4uy -> Informational 
        | 5uy -> Verbose       
        | n   -> Custom (n, "Unknown")

[<Flags>]
type EventProperty =
    | None           = 0x0us
    | Xml            = 0x1us
    | ForwardedXml   = 0x2us
    | LegacyEventLog = 0x4us
    | Reloggable     = 0x8us

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module EventProperty =
    let private valueLookup =
            [
                (EventProperty.None          , "None")
                (EventProperty.Xml           , "Xml")
                (EventProperty.ForwardedXml  , "ForwardedXml")
                (EventProperty.LegacyEventLog, "LegacyEventLog")
                (EventProperty.Reloggable    , "Reloggable")
                (LanguagePrimitives.EnumOfValue 0x010us  , "Unknown (0x10)")
                (LanguagePrimitives.EnumOfValue 0x020us  , "Unknown (0x20)")
                (LanguagePrimitives.EnumOfValue 0x040us  , "Unknown (0x40)")
                (LanguagePrimitives.EnumOfValue 0x080us  , "Unknown (0x80)")
                (LanguagePrimitives.EnumOfValue 0x100us  , "Unknown (0x100)")
                (LanguagePrimitives.EnumOfValue 0x200us  , "Unknown (0x200)")
                (LanguagePrimitives.EnumOfValue 0x400us  , "Unknown (0x400)")
                (LanguagePrimitives.EnumOfValue 0x800us  , "Unknown (0x800)")
                (LanguagePrimitives.EnumOfValue 0x1000us , "Unknown (0x1000)")
                (LanguagePrimitives.EnumOfValue 0x2000us , "Unknown (0x2000)")
                (LanguagePrimitives.EnumOfValue 0x4000us , "Unknown (0x4000)")
                (LanguagePrimitives.EnumOfValue 0x8000us , "Unknown (0x8000)")
            ]

    let fromUInt16 = LanguagePrimitives.EnumOfValue
    let info (x : EventProperty) =
        let str = 
            if x = EventProperty.None then "None" 
            else
                valueLookup
                |> Seq.choose (fun (v, name) -> if (x &&& v <> EventProperty.None) then Some name else None)
                |> String.concat " | "
        (uint16 x, str)

type EtlEvent() =
    member val is64bit       = false              with get, set
    member val flags         = EventFlag.None     with get, set
    member val eventProperty = EventProperty.None with get, set
    member val threadId      = 4u                 with get, set
    member val processId     = 4u                 with get, set
    member val timestamp     = 0L                 with get, set // Number of 100ns ticks relative to boot time
    member val provider      = Guid.Empty         with get, set
    member val eventId       = 0us                with get, set
    member val version       = 0uy                with get, set
    member val channel       = 0uy                with get, set
    member val level         = EventLevel.Verbose with get, set
    member val opcode        = 0uy                with get, set
    member val taskId        = 0us                with get, set
    member val keywords      = 0UL                with get, set
    member val kernelTime    = 0                  with get, set
    member val userTime      = 0                  with get, set
    member val activityId    = Guid.Empty         with get, set
    member val extendedData : ExtendedData list = []   with get, set
    member val payload      : byte array        = [||] with get, set

    member x.HeaderSize with get() = 0x50
    member x.Size with get() = x.HeaderSize + x.payload.Length // TODO: Add in extended data size
    member x.Clone() = x.MemberwiseClone() :?> EtlEvent

    member private x.Common (w:Util.ISerializer) size =
        let startOffset = w.Offset - 4L
        let endOffset = startOffset + (int64 size)

        w.EnumU16 "flags"      x.get_flags (EventFlag.fromUInt16 >> x.set_flags) EventFlag.info // 4
        w.EnumU16 "evtProp"    x.get_eventProperty (EventProperty.fromUInt16 >> x.set_eventProperty) EventProperty.info // 6
        w.UInt32  "tid"        x.get_threadId   x.set_threadId   // 8
        w.UInt32  "pid"        x.get_processId  x.set_processId  // c
        w.Int64   "timestamp"  x.get_timestamp  x.set_timestamp  // 10
        w.Guid    "provider"   x.get_provider   x.set_provider   // 18
        w.UInt16  "event"      x.get_eventId    x.set_eventId    // 28
        w.UInt8   "version"    x.get_version    x.set_version    // 2a
        w.UInt8   "channel"    x.get_channel    x.set_channel    // 2b
        w.EnumU8  "level"      x.get_level (EventLevel.fromUInt8 >> x.set_level) EventLevel.info // 2c
        w.UInt8   "opcode"     x.get_opcode     x.set_opcode     // 2d
        w.UInt16  "task"       x.get_taskId     x.set_taskId     // 2e
        w.UInt64  "keywords"   x.get_keywords   x.set_keywords   // 30
        w.Int32   "kTime"      x.get_kernelTime x.set_kernelTime // 38
        w.Int32   "uTime"      x.get_userTime   x.set_userTime   // 3c
        w.Guid    "activityId" x.get_activityId x.set_activityId // 40

        w.Meta "extendedData"
            (fun w -> x.extendedData |> Seq.iter (fun d -> d.Serialize w))
            (fun w ->
                if ((x.flags &&& EventFlag.ExtendedInfo) <> EventFlag.None) then
                    x.extendedData <- ExtendedData.Deserialize w) // TODO: Handle multiple

        let payloadSize = int <| (endOffset - w.Offset)
        if (payloadSize < 0) then failwith "Negative event payload size"
        if (payloadSize > 0) then 
            w.ByteArrayHex "payload" x.get_payload x.set_payload payloadSize // 50

        let paddingBytes = Util.paddingBytes x.Size
        if (paddingBytes > 0) then
            w.RepeatU8 "padding" 0uy paddingBytes

    member x.Serialize (w:Util.ISerializer) =
        let headerType = if x.is64bit then EtlHeaderType.EventHeader64 else EtlHeaderType.EventHeader64
        if (x.Size > int UInt16.MaxValue) then failwith "Payload too large"
        if (((x.flags &&& EventFlag.ExtendedInfo) = EventFlag.None) <> (x.extendedData.Length = 0)) then
            failwith "The ExtendedInfo event flag must be set if (and only if) there is extended data"

        w.Comment "Event"
        Util.writeValue w.UInt16 "size" (uint16 x.Size) // 0
        Util.writeValue w.EnumU16 "headerType" headerType EtlHeaderType.info // 2
        x.Common w x.Size

    static member Deserialize (w : Util.ISerializer) size headerType =
        let x = EtlEvent()

        x.is64bit <- 
            match headerType with 
            | EtlHeaderType.EventHeader32 -> false
            | EtlHeaderType.EventHeader64 -> true
            | headerType -> failwithf "Unexpected header type parsing event: %s" (EtlHeaderType.info headerType |> snd)

        x.Common w (int size)
        x

type BufferEvent =
    | System       of EtlSystemEvent
    | Event        of EtlEvent
    | TraceEvent   of EtlTraceEvent
    | InstanceGuid of EtlInstanceGuidEvent
    | Error

    member x.Size
        with get() =
            match x with
            | System y       -> y.Size
            | Event y        -> y.Size
            | TraceEvent y   -> y.Size
            | InstanceGuid y -> y.Size
            | Error          -> failwith "Error"

    member x.Timestamp
        with get() =
            match x with
            | System y       -> y.timestamp
            | Event y        -> y.timestamp
            | TraceEvent y   -> y.timestamp
            | InstanceGuid y -> y.timestamp
            | Error          -> failwith "Error"

    member x.Serialize (w : Util.ISerializer) =
        match x with
        | System y       -> y.Serialize w
        | Event y        -> y.Serialize w
        | TraceEvent y   -> y.Serialize w
        | InstanceGuid y -> y.Serialize w
        | Error          -> failwith "Error"

    static member Deserialize (w : Util.ISerializer) =
        let firstWord = Util.readValue w.UInt16 "size" 0us
        let headerType = EtlHeaderType.fromUInt16 (Util.readValue w.UInt16 "headerType" 0us)

        match headerType with
        | EtlHeaderType.System32
        | EtlHeaderType.System64
        | EtlHeaderType.Compact32
        | EtlHeaderType.Compact64
        | EtlHeaderType.PerfInfo32
        | EtlHeaderType.PerfInfo64    -> System (EtlSystemEvent.Deserialize w firstWord headerType)

        | EtlHeaderType.EventHeader32
        | EtlHeaderType.EventHeader64 -> Event (EtlEvent.Deserialize w firstWord headerType)

        | EtlHeaderType.FullHeader32
        | EtlHeaderType.FullHeader64  -> TraceEvent (EtlTraceEvent.Deserialize w firstWord headerType)

        | EtlHeaderType.Instance32
        | EtlHeaderType.Instance64    -> InstanceGuid (EtlInstanceGuidEvent.Deserialize w firstWord headerType)

        | EtlHeaderType.Error         -> Error
        | _ -> failwith "Unrecognised header type"
