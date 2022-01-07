namespace OpenEtw

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
    let fromUInt16 = LanguagePrimitives.EnumOfValue
    let info x =
        let str =
            match x with 
            | EtlHeaderType.System32      -> "System32"
            | EtlHeaderType.System64      -> "System64"
            | EtlHeaderType.Compact32     -> "Compact32"
            | EtlHeaderType.Compact64     -> "Compact64"
            | EtlHeaderType.PerfInfo32    -> "PerfInfo32"
            | EtlHeaderType.PerfInfo64    -> "PerfInfo64"
            | EtlHeaderType.Instance32    -> "Instance32"
            | EtlHeaderType.Instance64    -> "Instance64"
            | EtlHeaderType.EventHeader32 -> "EventHeader32"
            | EtlHeaderType.EventHeader64 -> "EventHeader64"
            | EtlHeaderType.FullHeader32  -> "FullHeader32"
            | EtlHeaderType.FullHeader64  -> "FullHeader64"
            | EtlHeaderType.Error         -> "Error"
            | x -> sprintf "Unknown (%d)" (uint16 x)
        (uint16 x, str)

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

    member private x.Common (w:Util.ISerializer) size =
        let hasContext, hasTiming =
            match x.headerType with
            | EtlHeaderType.System32   | EtlHeaderType.System64   -> true, true
            | EtlHeaderType.Compact32  | EtlHeaderType.Compact64  -> true, false
            | EtlHeaderType.PerfInfo32 | EtlHeaderType.PerfInfo64 -> false, true
            | _ -> failwith "Unexpected header type"

        w.UInt16 "hookId" x.get_hookId x.set_hookId            // 6
        if hasContext then
            w.UInt32 "tid" x.get_threadId x.set_threadId       // 8
            w.UInt32 "pid" x.get_processId x.set_processId     // C

        w.Int64 "timestamp" x.get_timestamp x.set_timestamp    // 8/10

        if hasTiming then
            w.UInt32 "kTime" x.get_kernelTime x.set_kernelTime // 18
            w.UInt32 "uTime" x.get_userTime   x.set_userTime   // 1C

        let payloadSize = (int size) - EtlHeaderType.size x.headerType
        if (payloadSize < 0) then failwith "Negative event payload size"
        if (payloadSize > 0) then
            w.ByteArrayHex "payload" x.get_payload x.set_payload payloadSize // 10/18/20

        let paddingBytes = Util.paddingBytes x.Size
        if (paddingBytes > 0) then
            w.RepeatU8 "padding" 0uy paddingBytes

    member x.Serialize (w:Util.ISerializer) =
        w.Comment "System Event"

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

        Util.writeValue w.UInt16 "version" x.version // 0
        Util.writeValue w.EnumU16 "headerType" x.headerType EtlHeaderType.info // 2
        Util.writeValue w.UInt16 "size" (uint16 x.Size) // 4
        x.Common w x.Size

    static member Deserialize (w : Util.ISerializer) version headerType =
        let x = EtlSystemEvent()

        x.version <- version
        x.headerType <- headerType
        let size = int (Util.readValue w.UInt16 "size" 0us)

        x.Common w size
        x
