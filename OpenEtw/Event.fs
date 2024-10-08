namespace OpenEtw
open System
open SerdesNet

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

    static member Deserialize (s : ISerializer) =
        let fullSize  = s.UInt16("fullSize",  0us)
        let dataType  = s.UInt16("dataType",  0us)
        let reserved2 = s.UInt16("reserved2", 0us) // Does this indicate if there's another extended data item following this one?
        let size = int (s.UInt16("size",      0us))

        if (int fullSize <> size + 8) then 
            failwith "Reserved1 was expected to be size + 8 when decoding extended event data"

        let arrayField (typeConstructor : byte array -> 'a) =
            let bytes = s.Bytes("", [||], size)
            typeConstructor bytes

        let value =
            match dataType with
            | 0x1us -> arrayField RelatedActivityId
            | 0x2us -> arrayField Sid
            | 0x3us -> arrayField TsId
            | 0x4us -> arrayField InstanceInfo
            | 0x5us -> 
                let matchId = s.UInt64("matchId", 0UL)
                let addresses = 
                    List.ofSeq <| seq { 
                        for _ in [8..4..size-1] do 
                            yield (s.UInt32("address", 0u))
                    }
                StackTrace32 (matchId, addresses)

            | 0x6us -> 
                let matchId = s.UInt64("matchId", 0UL)
                let addresses = 
                    List.ofSeq <| seq { 
                        for _ in [8..8..size-1] do 
                            yield (s.UInt64("address", 0UL))
                    }
                StackTrace64 (matchId, addresses)

            | 0x7us -> arrayField PebsIndex
            | 0x8us -> arrayField PmcCounter
            | 0x9us -> arrayField PsmKey
            | 0xaus -> arrayField EventKey
            | 0xbus -> arrayField EventSchemaTl
            | 0xcus -> arrayField ProvTraits
            | 0xdus -> arrayField ProcessStartKey
            | _ -> failwith "Unknown extended data type encountered"

        if(size % 8 <> 0) then
            s.Pad("", (Util.paddingBytes size), 0uy)

        [value] // TODO: Multiple extended data items

    member x.Serialize (s : ISerializer) =
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

        s.Comment name
        s.UInt16("reserved1", (uint16 size + 8us)) |> ignore
        s.UInt16("type",      enumValue) |> ignore
        s.UInt16("size",      (uint16 size)) |> ignore
        s.UInt16("reserved2", 0us) |> ignore

        let arrayField payload = s.Bytes("", payload, size) |> ignore

        match x with
        | RelatedActivityId p -> arrayField p
        | Sid p               -> arrayField p
        | TsId p              -> arrayField p
        | InstanceInfo p      -> arrayField p
        | StackTrace32 (batchId, addresses) ->
            s.UInt64("batchId", batchId) |> ignore
            for address in addresses do s.UInt32("address", address) |> ignore
        | StackTrace64 (batchId, addresses) ->
            s.UInt64("batchId", batchId) |> ignore
            for address in addresses do s.UInt64("address", address) |> ignore
        | PebsIndex p         -> arrayField p
        | PmcCounter p        -> arrayField p
        | PsmKey p            -> arrayField p
        | EventKey p          -> arrayField p
        | EventSchemaTl p     -> arrayField p
        | ProvTraits p        -> arrayField p
        | ProcessStartKey p   -> arrayField p

        let paddingBytes = Util.paddingBytes size
        if (paddingBytes > 0) then
            s.Pad("padding", paddingBytes, 0uy)

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

type EventLevelConverter() =
    interface IConverter<byte, EventLevel> with
        member x.FromNumeric v =
            match v with
            | 1uy -> Critical      
            | 2uy -> Error         
            | 3uy -> Warning       
            | 4uy -> Informational 
            | 5uy -> Verbose       
            | n   -> Custom (n, "Unknown")

        member x.ToNumeric v =
            match v with
            | Always        -> 0uy
            | Critical      -> 1uy
            | Error         -> 2uy
            | Warning       -> 3uy
            | Informational -> 4uy
            | Verbose       -> 5uy
            | Custom (n, _) -> n

        member x.FromSymbolic name = Custom (0xffuy, name)  
        member x.ToSymbolic v = 
            match v with
            | Always        -> "Always"
            | Critical      -> "Critical"
            | Error         -> "Error"
            | Warning       -> "Warning"
            | Informational -> "Informational"
            | Verbose       -> "Verbose"
            | Custom (_, s) -> s

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module EventLevel =
    let converter = EventLevelConverter() :> IConverter<byte, EventLevel>
    let toInt v = converter.ToNumeric v |> int
    let innerSerdes (n:string) (v:byte) (s:ISerializer) = s.UInt8(n, v, 0uy)
    let serdes (name : string) value (s : ISerializer) = s.Transform(name, value, innerSerdes, converter)

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

    member private x.Common (s : ISerializer) size =
        let startOffset = s.Offset - 4L
        let endOffset = startOffset + (int64 size)

        x.flags         <- s.EnumU16("flags",   x.flags) // 4
        x.eventProperty <- s.EnumU16("evtProp", x.eventProperty) // 6
        x.threadId   <- s.UInt32 ("tid",        x.threadId)   // 8
        x.processId  <- s.UInt32 ("pid",        x.processId)  // c
        x.timestamp  <- s.Int64  ("timestamp",  x.timestamp)  // 10
        x.provider   <- s.Guid   ("provider",   x.provider)   // 18
        x.eventId    <- s.UInt16 ("event",      x.eventId)    // 28
        x.version    <- s.UInt8  ("version",    x.version)    // 2a
        x.channel    <- s.UInt8  ("channel",    x.channel)    // 2b
        x.level      <- EventLevel.serdes "level" x.level s   // 2c
        x.opcode     <- s.UInt8  ("opcode",     x.opcode)     // 2d
        x.taskId     <- s.UInt16 ("task",       x.taskId)     // 2e
        x.keywords   <- s.UInt64 ("keywords",   x.keywords)   // 30
        x.kernelTime <- s.Int32  ("kTime",      x.kernelTime) // 38
        x.userTime   <- s.Int32  ("uTime",      x.userTime)   // 3c
        x.activityId <- s.Guid   ("activityId", x.activityId) // 40

        let extendedDataSerdes (_:string) (existing:ExtendedData list) (s1 : ISerializer) =
            let hasExtended = ((x.flags &&& EventFlag.ExtendedInfo) <> EventFlag.None)
            let result =
                match (s1.IsReading(), hasExtended) with
                | true, true -> ExtendedData.Deserialize s1 // TODO: Handle multiple
                | true, false -> []
                | false, _ ->
                    for ed in existing do
                        ed.Serialize s1
                    existing
            result

        x.extendedData <- s.Object("extendedData", x.extendedData, (fun a b c -> extendedDataSerdes a b c))

        let payloadSize = int <| (endOffset - s.Offset)
        if (payloadSize < 0) then failwith "Negative event payload size"
        if (payloadSize > 0) then 
            x.payload <- s.Bytes("payload", x.payload, payloadSize) // 50

        let paddingBytes = Util.paddingBytes x.Size
        if (paddingBytes > 0) then
            s.Pad("padding", paddingBytes, 0uy)

    member x.Serialize (s : ISerializer) =
        let headerType = if x.is64bit then EtlHeaderType.EventHeader64 else EtlHeaderType.EventHeader64
        if (x.Size > int UInt16.MaxValue) then failwith "Payload too large"
        if (((x.flags &&& EventFlag.ExtendedInfo) = EventFlag.None) <> (x.extendedData.Length = 0)) then
            failwith "The ExtendedInfo event flag must be set if (and only if) there is extended data"

        s.Comment "Event"
        s.UInt16("size", (uint16 x.Size)) |> ignore // 0
        s.EnumU16("headerType", headerType) |> ignore // 2
        x.Common s x.Size

    static member Deserialize (s : ISerializer) size headerType =
        let x = EtlEvent()

        x.is64bit <- 
            match headerType with 
            | EtlHeaderType.EventHeader32 -> false
            | EtlHeaderType.EventHeader64 -> true
            | headerType -> failwithf "Unexpected header type parsing event: %s" (string headerType)

        x.Common s (int size)
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

    member x.Serialize (s : ISerializer) =
        match x with
        | System y       -> y.Serialize s
        | Event y        -> y.Serialize s
        | TraceEvent y   -> y.Serialize s
        | InstanceGuid y -> y.Serialize s
        | Error          -> failwith "Error"

    static member Deserialize (s : ISerializer) =
        let firstWord = s.UInt16("size", 0us)
        let headerType = s.EnumU16("headerType", EtlHeaderType.Error)

        match headerType with
        | EtlHeaderType.System32
        | EtlHeaderType.System64
        | EtlHeaderType.Compact32
        | EtlHeaderType.Compact64
        | EtlHeaderType.PerfInfo32
        | EtlHeaderType.PerfInfo64    -> System (EtlSystemEvent.Deserialize s firstWord headerType)

        | EtlHeaderType.EventHeader32
        | EtlHeaderType.EventHeader64 -> Event (EtlEvent.Deserialize s firstWord headerType)

        | EtlHeaderType.FullHeader32
        | EtlHeaderType.FullHeader64  -> TraceEvent (EtlTraceEvent.Deserialize s firstWord headerType)

        | EtlHeaderType.Instance32
        | EtlHeaderType.Instance64    -> InstanceGuid (EtlInstanceGuidEvent.Deserialize s firstWord headerType)

        | EtlHeaderType.Error         -> Error
        | _ -> failwith "Unrecognised header type"
