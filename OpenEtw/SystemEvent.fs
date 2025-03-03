namespace OpenEtw
open SerdesNet

type EtlHeaderType =
    // System header
    | System32      = 0xC001us
    | System64      = 0xC002us
    | Compact32     = 0xC003us
    | Compact64     = 0xC004us
    | PerfInfo32    = 0xC010us
    | PerfInfo64    = 0xC011us
    // Instance Guid
    | Instance32    = 0xC00Bus
    | Instance64    = 0xC015us
    | Error         = 0xC00Dus
    // Event
    | EventHeader32 = 0xC012us
    | EventHeader64 = 0xC013us
    // Trace
    | FullHeader32  = 0xC00Aus
    | FullHeader64  = 0xC014us

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module EtlHeaderType =
    let size =
        function
        | EtlHeaderType.PerfInfo32
        | EtlHeaderType.PerfInfo64 -> 0x10
        | EtlHeaderType.Compact32
        | EtlHeaderType.Compact64 -> 0x18
        | EtlHeaderType.System32
        | EtlHeaderType.System64 -> 0x20
        | x -> failwithf "Header type with unknown size encountered (%d)" (uint16 x)

type EtlSystemEvent() =
    member val version    = 2us                    with get, set
    member val headerType = EtlHeaderType.System32 with get, set
    member val hookId     = 0us                    with get, set
    member val threadId   = 0u                     with get, set // Always 0 for perf events
    member val processId  = 0u                     with get, set // Always 0 for perf events
    member val timestamp  = 0L                     with get, set
    member val kernelTime = 0u                     with get, set // 0 for compact system events
    member val userTime   = 0u                     with get, set // 0 for compact system events
    member val payload : byte array = [||]         with get, set

    member x.Size with get() = EtlHeaderType.size x.headerType + x.payload.Length
    member x.Clone() = x.MemberwiseClone() :?> EtlSystemEvent

    member private x.Serdes (s:ISerializer) size =
        let hasContext, hasTiming =
            match x.headerType with
            | EtlHeaderType.System32   | EtlHeaderType.System64   -> true, true
            | EtlHeaderType.Compact32  | EtlHeaderType.Compact64  -> true, false
            | EtlHeaderType.PerfInfo32 | EtlHeaderType.PerfInfo64 -> false, true
            | _ -> failwith "Unexpected header type"

        x.hookId <- s.UInt16("hookId", x.hookId) // 6
        if hasContext then
            x.threadId <- s.UInt32("tid", x.threadId)       // 8
            x.processId <- s.UInt32("pid", x.processId)     // C

        x.timestamp <- s.Int64("timestamp", x.timestamp)    // 8/10

        if hasTiming then
            x.kernelTime <- s.UInt32("kTime", x.kernelTime) // 18
            x.userTime   <- s.UInt32("uTime", x.userTime)   // 1C

        let payloadSize = (int size) - EtlHeaderType.size x.headerType
        if (payloadSize < 0) then failwith "Negative event payload size"
        if (payloadSize > 0) then
            x.payload <- s.Bytes("payload", x.payload, payloadSize) // 10/18/20

        let paddingBytes = Util.paddingBytes x.Size
        if (paddingBytes > 0) then
            s.Pad(paddingBytes)

    member x.Serialize (s:ISerializer) =
        s.Comment "System Event"

        match x.headerType with // Sanity checks
        | EtlHeaderType.System32
        | EtlHeaderType.System64 -> ()
        | EtlHeaderType.Compact32
        | EtlHeaderType.Compact64 ->
            if (x.kernelTime <> 0u || x.userTime <> 0u) then failwith "Compact system events cannot have kernel/user timing info"
        | EtlHeaderType.PerfInfo32
        | EtlHeaderType.PerfInfo64 ->
            if (x.threadId <> 0u || x.processId <> 0u) then failwith "PerfInfo events cannot have thread and process ids"
        | _ -> failwith "System event with invalid header type found"

        x.version <- s.UInt16("version", x.version) // 0
        x.headerType <- s.EnumU16("headerType", x.headerType) // 2
        s.UInt16("size", (uint16 x.Size)) |> ignore // 4
        x.Serdes s x.Size

    static member Deserialize (s : ISerializer) version headerType =
        let x = EtlSystemEvent()

        x.version <- version
        x.headerType <- headerType
        let size = int (s.UInt16("size", 0us))

        x.Serdes s size
        x
