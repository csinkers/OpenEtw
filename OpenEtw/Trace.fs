namespace OpenEtw
open System

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

    member private x.Common (w:Util.ISerializer) =
        let systemHeaderSize = 0x4c
        let startOffset = w.Offset

        w.Int16 "unk00s" x.get_unk00s x.set_unk00s // 0
        w.Int16 "unk02s" x.get_unk02s x.set_unk02s // 2
        w.Int32 "unk04i" x.get_unk04i x.set_unk04i // 4
        w.Int32 "unk08i" x.get_unk08i x.set_unk08i // 8
        w.Int32 "unk0ci" x.get_unk0ci x.set_unk0ci // c
        w.Int64 "unk10l" x.get_unk10l x.set_unk10l // 10
        w.Int64 "unk18l" x.get_unk18l x.set_unk18l // 18
        w.Int64 "unk20l" x.get_unk20l x.set_unk20l // 20
        w.Int32 "memoryBufferSize" x.get_memoryBufferSize x.set_memoryBufferSize // 28

        let eventBuffersOffset = w.Offset // Save the offset so we can overwrite it once we know how many buffers were written
        let eventBuffers = Util.readValue w.Int32 "eventBuffers" 0            // 2c

        w.Int32 "unk30i" x.get_unk30i x.set_unk30i                            // 30
        w.Int16 "unk34s" x.get_unk34s x.set_unk34s                            // 34
        w.Int16 "logicalCpuCount" x.get_logicalCpuCount x.set_logicalCpuCount // 36
        w.Int64 "unk38l" x.get_unk38l x.set_unk38l                            // 38
        w.Int64 "unk40l" x.get_unk40l x.set_unk40l                            // 40

        // <SYSTEM_TRACE_HEADER>
        w.Int16 "etlVersion" x.get_etlVersion x.set_etlVersion // 48
        w.EnumU8 "headerType" x.get_headerType (HeaderType.fromUInt8 >> x.set_headerType) HeaderType.info // 4a
        w.UInt8 "flags" x.get_flags x.set_flags // 4b

        let mutable headerSize = 0
        w.Int32 "eventDataHeaderSize" // 4c
            (fun () -> 
                let sessionNameLength = x.sessionName.Length
                let fileNameLength    = x.fileName.Length
                headerSize <- 0x13c + sessionNameLength + fileNameLength
                headerSize)
            (fun v -> headerSize <- v) // TODO: Assert this after reading the header

        w.UInt32 "threadId"     x.get_threadId     x.set_threadId     // 50
        w.UInt32 "processId"    x.get_processId    x.set_processId    // 54
        w.Int64  "userSpacePtr" x.get_userSpacePtr x.set_userSpacePtr // 58
        w.Int32  "kernelTime"   x.get_kernelTime   x.set_kernelTime   // 60
        w.Int32  "userTime"     x.get_userTime     x.set_userTime     // 64

        // </SYSTEM_TRACE_HEADER>
        // <_TRACE_LOGFILE_HEADER> https://msdn.microsoft.com/en-us/library/windows/desktop/aa364145(v=vs.85).aspx
        // Also KernelTraceEventParser.mof -> Event Trace Event (type 0)
        w.Int32 "bufferSize" x.get_bufferSize x.set_bufferSize                    // 68
        w.Int32 "version" x.get_version x.set_version // major.minor.sub.subminor // 6c
        w.Int32 "providerVersion" x.get_providerVersion x.set_providerVersion     // 70
        w.Int32 "physicalCpuCount" x.get_physicalCpuCount x.set_physicalCpuCount  // 74

        w.Int64 "endTime" // 78
            (fun () -> match x.endTime with
                       | Some time -> (Util.toEtlAbsoluteTicks time)
                       | None -> 0L)
            (fun v -> x.endTime <- match v with
                                   | 0L -> None
                                   | x -> Some (Util.fromEtlAbsoluteTicks x))

        w.Int32 "timerResolution" x.get_timerResolution x.set_timerResolution // 80
        w.Int32 "maxFileSizeMB" x.get_maxFileSizeMB x.set_maxFileSizeMB       // 84
        w.EnumU32 "logFileMode" x.get_logFileMode (LogFileMode.fromUInt32 >> x.set_logFileMode) LogFileMode.info // 88

        let totalBuffersOffset = w.Offset // Save the offset so we can overwrite it once we know how many buffers were written
        let totalBuffers = Util.readValue w.Int32 "totalBuffers" 0 // 8c

        w.Int32 "startBuffers"  x.get_startBuffers  x.set_startBuffers  // 90
        w.Int32 "pointerSize"   x.get_pointerSize   x.set_pointerSize   // 94
        w.Int32 "eventsLost"    x.get_eventsLost    x.set_eventsLost    // 98
        w.Int32 "cpuSpeed"      x.get_cpuSpeed      x.set_cpuSpeed      // 9c
        w.Int64 "loggerNameId"  x.get_loggerNameId  x.set_loggerNameId  // a0
        w.Int64 "logFileNameId" x.get_logFileNameId x.set_logFileNameId // a8
        w.Int32 "unkb0i"        x.get_unkb0i        x.set_unkb0i        // b0 (something timezone related, have seen value of -60 i.e. 0xffffffc4)

        // TIME_ZONE_INFORMATION
        w.FixedLengthString "timezone1" x.get_timezone1 x.set_timezone1 EtlTrace.timeZoneLength // b4
        w.FixedLengthString "timezone2" x.get_timezone2 x.set_timezone2 EtlTrace.timeZoneLength // 108
        w.RepeatU8 "padding" 0uy 4 // 15c
        w.Int64 "bootTime" (x.get_bootTime >> Util.toEtlAbsoluteTicks) (Util.fromEtlAbsoluteTicks >> x.set_bootTime)    // 160
        w.UInt64 "perfCounterFreq" x.get_perfCounterFreq x.set_perfCounterFreq                                          // 168
        w.Int64 "startTime" (x.get_startTime >> Util.toEtlAbsoluteTicks) (Util.fromEtlAbsoluteTicks >> x.set_startTime) // 170
        w.UInt32 "reservedFlags"             x.get_reservedFlags x.set_reservedFlags                                    // 178
        w.UInt32 "buffersLost"               x.get_buffersLost   x.set_buffersLost                                      // 17c
        w.NullTerminatedString "sessionName" x.get_sessionName   x.set_sessionName                                      // 180
        w.NullTerminatedString "fileName"    x.get_fileName      x.set_fileName                                         // ??
        // </_TRACE_LOGFILE_HEADER>

        let headerLength = int (w.Offset - startOffset)
        let paddingBytes = x.bufferSize - headerLength // (EtlTrace.headerSize + sessionNameBytes.Length + fileNameBytes.Length + 4) // 4 = 2*null terminators

        match paddingBytes with
        | x when x < 0 -> failwith "Overlength trace header"
        | x when x > 0 ->
            let alignmentBytes = x % 8
            if (alignmentBytes > 0) then
                w.RepeatU8 "alignment" 0uy alignmentBytes

            w.RepeatU8 "padding" 0xFFuy ((x/8)*8)
        | _ -> ()

        w.Meta "buffers"
            (fun w ->
                let mutable numBuffers = 0
                x.buffers |> Seq.iteri (fun i b ->
                    w.NewLine()
                    w.Comment (sprintf "Buffer %d" i)
                    b.Serialize w
                    numBuffers <- numBuffers + 1
                )

                w.Seek eventBuffersOffset
                Util.writeValue w.Int32 "eventBuffers" numBuffers

                w.Seek totalBuffersOffset
                Util.writeValue  w.Int32 "totalBuffers" (numBuffers + 1)
            )
            
            (fun w ->
                let startOffset = w.Offset
                let buffers = seq {
                    w.Seek startOffset
                    for i in [1..totalBuffers-1] do
                        yield EtlBuffer.Deserialize w
                }
                x.buffers <- buffers)

    member x.Serialize (w:Util.ISerializer) = x.Common w
    static member Deserialize (w:Util.ISerializer) =
        let x = EtlTrace()
        x.Common w
        x
