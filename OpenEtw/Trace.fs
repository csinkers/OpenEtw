namespace OpenEtw
open System
open SerdesNet

[<Flags>]
type LogFileMode =
    | None                    = 0x00000000u
    | Sequential              = 0x00000001u
    | Circular                = 0x00000002u
    | Append                  = 0x00000004u
    | NewFile                 = 0x00000008u
    | UseMsFlushTimer         = 0x00000010u
    | Preallocate             = 0x00000020u
    | NonStoppable            = 0x00000040u
    | Secure                  = 0x00000080u
    | RealTime                = 0x00000100u
    | DelayOpenFile           = 0x00000200u
    | Buffering               = 0x00000400u
    | PrivateLogger           = 0x00000800u
    | AddHeader               = 0x00001000u
    | UseKbForSize            = 0x00002000u
    | UseGlobalSequence       = 0x00004000u
    | UseLocalSequence        = 0x00008000u
    | RelogMode               = 0x00010000u
    | PrivateInProc           = 0x00020000u
    | BufferInterface         = 0x00040000u
    | KdFilter                = 0x00080000u
    | RealTimeRelog           = 0x00100000u
    | LostEventsDebug         = 0x00200000u
    | StopOnHybridShutdown    = 0x00400000u
    | PersistOnHybridShutdown = 0x00800000u
    | UsePagedMemory          = 0x01000000u
    | SystemLogger            = 0x02000000u
    | Compressed              = 0x04000000u
    | IndependentSession      = 0x08000000u
    | NoPerProcessorBuffering = 0x10000000u
    | Blocking                = 0x20000000u
    | AddToTriageDump         = 0x80000000u

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module LogFileMode =
    let private valueLookup =
            [
                (LogFileMode.Sequential,              "Sequential")
                (LogFileMode.Circular,                "Circular")
                (LogFileMode.Append,                  "Append")
                (LogFileMode.NewFile,                 "NewFile")
                (LogFileMode.UseMsFlushTimer,         "UseMsFlushTimer")
                (LogFileMode.Preallocate,             "Preallocate")
                (LogFileMode.NonStoppable,            "NonStoppable")
                (LogFileMode.Secure,                  "Secure")
                (LogFileMode.RealTime,                "RealTime")
                (LogFileMode.DelayOpenFile,           "DelayOpenFile")
                (LogFileMode.Buffering,               "Buffering")
                (LogFileMode.PrivateLogger,           "PrivateLogger")
                (LogFileMode.AddHeader,               "AddHeader")
                (LogFileMode.UseKbForSize,            "UseKbForSize")
                (LogFileMode.UseGlobalSequence,       "UseGlobalSequence")
                (LogFileMode.UseLocalSequence,        "UseLocalSequence")
                (LogFileMode.RelogMode,               "RelogMode")
                (LogFileMode.PrivateInProc,           "PrivateInProc")
                (LogFileMode.BufferInterface,         "BufferInterface")
                (LogFileMode.KdFilter,                "KdFilter")
                (LogFileMode.RealTimeRelog,           "RealTimeRelog")
                (LogFileMode.LostEventsDebug,         "LostEventsDebug")
                (LogFileMode.StopOnHybridShutdown,    "StopOnHybridShutdown")
                (LogFileMode.PersistOnHybridShutdown, "PersistOnHybridShutdown")
                (LogFileMode.UsePagedMemory,          "UsePagedMemory")
                (LogFileMode.SystemLogger,            "SystemLogger")
                (LogFileMode.Compressed,              "Compressed")
                (LogFileMode.IndependentSession,      "IndependentSession")
                (LogFileMode.NoPerProcessorBuffering, "NoPerProcessorBuffering")
                (LogFileMode.Blocking,                "Blocking")
                (LanguagePrimitives.EnumOfValue 0x40000000u, "Unknown (0x4000_0000)")
                (LogFileMode.AddToTriageDump,         "AddToTriageDump")
            ]

    let fromUInt32 = LanguagePrimitives.EnumOfValue
    let info x =
        if x = LogFileMode.None then 0u, "None" else

        let str =
            valueLookup
            |> Seq.choose (fun (v, name) -> if (x &&& v <> LogFileMode.None) then Some name else None)
            |> String.concat " | "

        (uint32 x, str)

type HeaderType =
    | Full32Bit = 1uy
    | Full64Bit = 2uy
    | Compact32Bit = 3uy
    | Compact64Bit = 4uy

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module HeaderType =
    let fromUInt8 = LanguagePrimitives.EnumOfValue
    let info x =
        let str =
            match x with
            | HeaderType.Full32Bit    -> "Full32Bit"
            | HeaderType.Full64Bit    -> "Full64Bit"
            | HeaderType.Compact32Bit -> "Compact32Bit"
            | HeaderType.Compact64Bit -> "Compact64Bit"
            | _ -> "Unknown"
        (uint8 x, str)

type EtlTrace() =
    static member private timeZoneLength = 0x54

    // System header
    member val memoryBufferSize = 1024 * 1024                                with get, set
    member val logicalCpuCount  = System.Environment.ProcessorCount |> int16 with get, set
    member val physicalCpuCount = System.Environment.ProcessorCount |> int   with get, set
    member val etlVersion       = 2s    with get, set // Version. Valid values: 0..255. Semantic values are at least 1,2,3
    member val unk00s           = 0s    with get, set
    member val unk02s           = 1s    with get, set
    member val unk04i           = 0x1b8 with get, set
    member val unk08i           = 0x1b8 with get, set // Usually 0x1b8 or so, sometimes 0
    member val unk0ci           = 0     with get, set
    member val unk10l           = 0L    with get, set
    member val unk18l           = 0L    with get, set
    member val unk20l           = 0L    with get, set
    member val unk30i           = 0x1b8 with get, set // Usually 0x1b8, probably a length or offset. Large values are invalid.
    member val unk34s           = 0x21s with get, set
    member val unk38l           = 0L    with get, set
    member val unk40l           = 0L    with get, set
    member val headerType       = HeaderType.Full64Bit with get, set
    member val flags            = 0xc0uy               with get, set

    // EventData header
    member val threadId        = 0u         with get, set
    member val processId       = 0u         with get, set
    member val userSpacePtr    = 0L         with get, set
    member val kernelTime      = 1          with get, set // KernelTime / 15
    member val userTime        = 1          with get, set // UserTime / 15
    member val version         = 0x05010306 with get, set // e.g. 6.3.1.5
    member val providerVersion = 9600       with get, set // Build

    member val timerResolution = 156250        with get, set // Typically 156,250
    member val perfCounterFreq = 10000000UL    with get, set
    member val bootTime        = Util.etlEpoch with get, set
    member val startTime       = Util.etlEpoch with get, set
    member val endTime : System.DateTime option = None with get, set

    member val maxFileSizeMB   = 0    with get, set
    member val logFileMode     = LogFileMode.Sequential with get, set
    member val pointerSize     = 8    with get, set
    member val startBuffers    = 1    with get, set
    member val eventsLost      = 0    with get, set
    member val cpuSpeed        = 2208 with get, set

    member val sessionName     = ""                with get, set
    member val fileName        = ""                with get, set
    member val timezone1       = "@tzres.dll,-932" with get, set
    member val timezone2       = "@tzres.dll,-931" with get, set

    member val loggerNameId    = 0xaL with get, set
    member val logFileNameId   = 0x7L with get, set

    member val reservedFlags   = 1u with get, set
    member val buffersLost     = 0u with get, set

    member val bufferSize      = 0x10000 with get, set // 64k by default. Merged etls can be 1MB etc.
    member val unkb0i          = 0       with get, set

    member val buffers : EtlBuffer seq = Seq.empty with get, set

    static member private headerSize = 0x180

    member x.Clone() = x.MemberwiseClone() :?> EtlTrace

    member private x.Common (s : ISerdes) =
        let systemHeaderSize = 0x4c
        let startOffset = s.Offset

        x.unk00s <- s.Int16("unk00s", x.unk00s) // 0
        x.unk02s <- s.Int16("unk02s", x.unk02s) // 2
        x.unk04i <- s.Int32("unk04i", x.unk04i) // 4
        x.unk08i <- s.Int32("unk08i", x.unk08i) // 8
        x.unk0ci <- s.Int32("unk0ci", x.unk0ci) // c
        x.unk10l <- s.Int64("unk10l", x.unk10l) // 10
        x.unk18l <- s.Int64("unk18l", x.unk18l) // 18
        x.unk20l <- s.Int64("unk20l", x.unk20l) // 20
        x.memoryBufferSize <- s.Int32("memoryBufferSize", x.memoryBufferSize) // 28

        let eventBuffersOffset = s.Offset // Save the offset so we can overwrite it once we know how many buffers were written
        let eventBuffers = s.Int32("eventBuffers", 0) // 2c

        x.unk30i <- s.Int32("unk30i", x.unk30i)                            // 30
        x.unk34s <- s.Int16("unk34s", x.unk34s)                            // 34
        x.logicalCpuCount <- s.Int16("logicalCpuCount", x.logicalCpuCount) // 36
        x.unk38l <- s.Int64("unk38l", x.unk38l)                            // 38
        x.unk40l <- s.Int64("unk40l", x.unk40l)                            // 40

        // <SYSTEM_TRACE_HEADER>
        x.etlVersion <- s.Int16("etlVersion", x.etlVersion) // 48
        x.headerType <- s.EnumU8("headerType", x.headerType) // 4a
        x.flags <- s.UInt8("flags", x.flags) // 4b

        let sessionNameLength = x.sessionName.Length
        let fileNameLength    = x.fileName.Length
        let headerSize = s.Int32("eventDataHeaderSize", 0x13c + sessionNameLength + fileNameLength) // 4c

        x.threadId     <- s.UInt32("threadId",     x.threadId)     // 50
        x.processId    <- s.UInt32("processId",    x.processId)    // 54
        x.userSpacePtr <- s.Int64 ("userSpacePtr", x.userSpacePtr) // 58
        x.kernelTime   <- s.Int32 ("kernelTime",   x.kernelTime)   // 60
        x.userTime     <- s.Int32 ("userTime",     x.userTime)     // 64

        // </SYSTEM_TRACE_HEADER>
        // <_TRACE_LOGFILE_HEADER> https://msdn.microsoft.com/en-us/library/windows/desktop/aa364145(v=vs.85).aspx
        // Also KernelTraceEventParser.mof -> Event Trace Event (type 0)
        x.bufferSize <- s.Int32("bufferSize", x.bufferSize)                    // 68
        x.version <- s.Int32("version", x.version) // major.minor.sub.subminor // 6c
        x.providerVersion <- s.Int32("providerVersion", x.providerVersion)     // 70
        x.physicalCpuCount <- s.Int32("physicalCpuCount", x.physicalCpuCount)  // 74

        x.endTime <-  // 78
            match s.Int64("endTime",
                          match x.endTime with
                          | Some time -> (Util.toEtlAbsoluteTicks time)
                          | None -> 0L) with
            | 0L -> None
            | x -> Some (Util.fromEtlAbsoluteTicks x)

        x.timerResolution <- s.Int32("timerResolution", x.timerResolution) // 80
        x.maxFileSizeMB <- s.Int32("maxFileSizeMB", x.maxFileSizeMB)       // 84
        x.logFileMode <- s.EnumU32("logFileMode", x.logFileMode)  // 88

        let totalBuffersOffset = s.Offset // Save the offset so we can overwrite it once we know how many buffers were written
        let totalBuffers = s.Int32("totalBuffers", 0) // 8c

        x.startBuffers  <- s.Int32("startBuffers",  x.startBuffers)  // 90
        x.pointerSize   <- s.Int32("pointerSize",   x.pointerSize)   // 94
        x.eventsLost    <- s.Int32("eventsLost",    x.eventsLost)    // 98
        x.cpuSpeed      <- s.Int32("cpuSpeed",      x.cpuSpeed)      // 9c
        x.loggerNameId  <- s.Int64("loggerNameId",  x.loggerNameId)  // a0
        x.logFileNameId <- s.Int64("logFileNameId", x.logFileNameId) // a8
        x.unkb0i        <- s.Int32("unkb0i",        x.unkb0i)        // b0 (something timezone related, have seen value of -60 i.e. 0xffffffc4)

        let serdesTime (name:string) (serializer:ISerdes) v = Util.fromEtlAbsoluteTicks <| serializer.Int64(name, (v |> Util.toEtlAbsoluteTicks))

        // TIME_ZONE_INFORMATION
        x.timezone1 <- s.FixedLengthString("timezone1", x.timezone1, EtlTrace.timeZoneLength) // b4
        x.timezone2 <- s.FixedLengthString("timezone2", x.timezone2, EtlTrace.timeZoneLength) // 108
        s.Pad(4) // 15c
        x.bootTime        <- serdesTime "bootTime" s x.bootTime                   // 160
        x.perfCounterFreq <- s.UInt64("perfCounterFreq", x.perfCounterFreq)       // 168
        x.startTime       <- serdesTime "startTime" s x.startTime                 // 170
        x.reservedFlags   <- s.UInt32("reservedFlags",             x.reservedFlags) // 178
        x.buffersLost     <- s.UInt32("buffersLost",               x.buffersLost)   // 17c
        x.sessionName     <- s.NullTerminatedString("sessionName", x.sessionName)   // 180
        x.fileName        <- s.NullTerminatedString("fileName",    x.fileName)      // ??
        // </_TRACE_LOGFILE_HEADER>

        let headerLength = int (s.Offset - startOffset)
        let paddingBytes = x.bufferSize - headerLength // (EtlTrace.headerSize + sessionNameBytes.Length + fileNameBytes.Length + 4) // 4 = 2*null terminators

        match paddingBytes with
        | x when x < 0 -> failwith "Overlength trace header"
        | x when x > 0 ->
            let alignmentBytes = x % 8
            if (alignmentBytes > 0) then
                s.Pad(alignmentBytes)

            s.Pad(((x/8)*8), 0xFFuy)
        | _ -> ()

        let meta (s:ISerdes) (name:string) reader writer =
            s.Begin(name)
            if s.IsReading()
            then reader s
            else writer s
            s.End()

        meta s "buffers"
            (fun s -> // Read
                let startOffset = s.Offset
                let buffers = seq {
                    s.Seek startOffset
                    for i in [1..totalBuffers-1] do
                        yield EtlBuffer.Deserialize s
                }
                x.buffers <- buffers)

            (fun s -> // Write
                let mutable numBuffers = 0
                x.buffers |> Seq.iteri (fun i b ->
                    s.NewLine()
                    s.Comment (sprintf "Buffer %d" i)
                    b.Serialize s
                    numBuffers <- numBuffers + 1
                )

                s.Seek eventBuffersOffset
                s.Int32("eventBuffers", numBuffers) |> ignore

                s.Seek totalBuffersOffset
                s.Int32("totalBuffers", (numBuffers + 1)) |> ignore
            )

    member x.Serialize (s : ISerdes) = x.Common s
    static member Deserialize (s : ISerdes) =
        let x = EtlTrace()
        x.Common s
        x
