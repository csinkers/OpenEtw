module Tests

open System.Diagnostics
open System.IO
open System.Xml.Linq
open FSharp.Data
open FsUnit
open SerdesNet
open Xunit
open OpenEtw
type TraceReportXml = XmlProvider<"SampleTraceReport.xml">
let providerGuid = System.Guid.Parse("e6467579-e015-4d5a-be75-ddaac9c1a96e")

let writeEtl (trace : EtlTrace) (stream : Stream) =
    use binaryWriter = new BinaryWriter(stream)
    use writer = Util.buildWriter binaryWriter

    use ms = new MemoryStream()
    use tw = new StreamWriter(ms)
    use annotated = new AnnotationProxySerdes(writer, tw, fun s -> System.Text.Encoding.UTF8.GetBytes(s))
    trace.Serialize annotated

    tw.Flush()
    ms.Position <- 0L
    use tr = new StreamReader(ms)
    tr.ReadToEnd()

let readEtl (stream : Stream) =
    use binaryReader = new BinaryReader(stream)
    use reader = Util.buildReader binaryReader

    use ms = new MemoryStream()
    use tw = new StreamWriter(ms)
    use annotated = new AnnotationProxySerdes(reader, tw, fun s -> System.Text.Encoding.UTF8.GetBytes(s))
    let trace = EtlTrace.Deserialize annotated

    tw.Flush()
    ms.Position <- 0L
    use tr = new StreamReader(ms)
    (trace, tr.ReadToEnd())

let roundTripTrace (trace : EtlTrace) =
    use ms = new MemoryStream()
    let writeNotes = writeEtl trace ms
    ms.Seek(0L, SeekOrigin.Begin) |> ignore
    let (trace, readNotes) = readEtl ms
    (trace, writeNotes, readNotes)

type ExeResult =
    | NormalExit of int
    | Timeout

let runExe exe args (timeout:System.TimeSpan) =
    let psi = ProcessStartInfo(exe, args)
    psi.RedirectStandardOutput <- true
    psi.RedirectStandardError  <- true
    psi.UseShellExecute <- false
    psi.CreateNoWindow  <- true
    use p = Process.Start(psi)

    let stdOut = p.StandardOutput.ReadToEnd()
    let stdErr = p.StandardError.ReadToEnd()

    match p.WaitForExit(int timeout.TotalMilliseconds) with
    | true -> (NormalExit p.ExitCode, stdOut.ToString(), stdErr.ToString())
    | false -> 
        try
            p.Kill()
        with | _ -> ()
        (Timeout, stdOut.ToString(), stdErr.ToString())

let generateTraceReport filename =
    let tempFile = Path.GetTempFileName()
    let timeoutSeconds = 60
    let exeResult, stdout, stderr = runExe "tracerpt" (sprintf "-y -of XML \"%s\" -o \"%s\"" filename tempFile) (System.TimeSpan.FromSeconds(double timeoutSeconds))
    match exeResult with
    | NormalExit 0 ->
        let content = 
            use stream = File.Open(tempFile, FileMode.Open)
            TraceReportXml.Load stream
        File.Delete tempFile
        content
    | Timeout -> failwithf "Trace report generation timed out after %d seconds" timeoutSeconds
    | _ -> failwithf "Error generating trace report:\n%s\n%s" stdout stderr

let saveAndGenerateTraceReport trace =
    let etlFilename = Path.GetTempFileName()
    let writeToFile path =
        use stream = File.Open(path, FileMode.Create)
        writeEtl trace stream

    let writeNotes = writeToFile etlFilename
    let report = generateTraceReport etlFilename
    File.Delete etlFilename
    (report, writeNotes)

//[<FsCheck.Property>]
let ``ETW data can be roundtripped through an ETL file`` (trace : EtlTrace) = 
    let (result, writeNotes, readNotes) = roundTripTrace trace
    trace = result
    
//[<FsCheck.Property>]
let ``ETL files can be parsed by tracerpt`` = ()

//[<FsCheck.Property>]
let ``ETL files can be merged with xperf`` = ()

type LoaderTests() =
    let ``Load single 32 bit normal event ETL`` = ()
    let ``Load single 64 bit normal event ETL`` = ()
    let ``Load single 32 bit system event ETL`` = ()
    let ``Load single 64 bit system event ETL`` = ()
    let ``Load single 32 bit compact system event ETL`` = ()
    let ``Load single 64 bit compact system event ETL`` = ()
    let ``Load single 32 bit perfinfo event ETL`` = ()
    let ``Load single 64 bit perfinfo event ETL`` = ()
    let ``Load single 32 bit instance event ETL`` = ()
    let ``Load single 64 bit instance event ETL`` = ()
    let ``Load ETL with two buffers`` = ()

type EventDefinition = System.Guid * uint16 * EventLevel * uint16 * uint8 * uint64

let buildEvent is64bit (provider, eventId, level, task, opcode, keywords) time =
    let event = EtlEvent()
    event.is64bit <- is64bit
    event.provider <- provider
    event.eventId <- eventId
    event.level <- level
    event.taskId <- task
    event.opcode <- opcode
    event.keywords <- keywords
    event.timestamp <- Util.toEtlAbsoluteTicks time
    event

let buildTrace is64bit bootTime startTime events =
    let trace = EtlTrace()
    trace.pointerSize <- if is64bit then 8 else 4
    trace.buffers <- EtlBuffer.BuildBuffers trace.bufferSize (events |> Seq.map (fun e -> e, 0uy))
    trace.bootTime <- bootTime
    trace.startTime <- startTime
    trace

let getEvent (report:TraceReportXml.Events) predicate = report.Events |> Seq.where predicate |> Seq.exactlyOne
let pNormal (provider:System.Guid) (eventId:int) = fun (e:TraceReportXml.Event) -> e.System.Provider.Guid = Some provider && e.System.EventId = eventId
let traceNs = "http://schemas.microsoft.com/win/2004/08/events/trace"
let pInstance (e:TraceReportXml.Event) =
    let eti = e.XElement.Element(XName.Get("ExtendedTracingInfo", traceNs))
    let iid = eti.Element(XName.Get("InstanceID", traceNs))
    iid <> null

type TraceReportTests() =
    [<Fact>]
    member x.``Generate single 32 bit normal event ETL``() = 
        let time = System.DateTime.UtcNow
        let eventDefinition = (providerGuid, 6us, EventLevel.Informational, 4us, 19uy, 0x8UL)
        let event = buildEvent false eventDefinition time
        let trace = buildTrace false (time.AddHours(-1.0)) time [Event event]
        let (traceReport, writeNotes) = saveAndGenerateTraceReport trace

        traceReport.Events |> should haveLength 2
        let e = (getEvent traceReport (pNormal providerGuid 6))
        let s =  e.System
        s.Provider.Guid |> should equal (Some providerGuid)
        s.EventId |> should equal 6
        s.Level |> should equal (EventLevel.toInt EventLevel.Informational)
        s.Task |> should equal 4
        s.Opcode |> should equal 19
        s.Keywords |> should equal "0x8"

    [<Fact>]
    member x.``Generate single 64 bit normal event ETL``() = 
        let time = System.DateTime.UtcNow
        let eventDefinition = (providerGuid, 6us, EventLevel.Informational, 4us, 19uy, 0x8UL)
        let event = buildEvent true eventDefinition time
        let trace = buildTrace true (time.AddHours(-1.0)) time [Event event]
        let (traceReport, writeNotes) = saveAndGenerateTraceReport trace

        traceReport.Events |> should haveLength 2
        let e = (getEvent traceReport (pNormal providerGuid 6))
        let s =  e.System
        s.Provider.Guid |> should equal (Some providerGuid)
        s.EventId |> should equal 6
        s.Level |> should equal (EventLevel.toInt EventLevel.Informational)
        s.Task |> should equal 4
        s.Opcode |> should equal 19
        s.Keywords |> should equal "0x8"
(*
    [<Fact>]
    member x.``Generate single 32 bit system event ETL``() =
        let time = System.DateTime.UtcNow
        let event = new EtlSystemEvent()
        event.headerType <- EtlHeaderType.System32
        event.hookId <- 1us
        event.timestamp <- Util.toEtlAbsoluteTicks time
        event.payload <- [||]
        let trace = buildTrace false (time.AddHours(-1.0)) time [System event]
        let (traceReport, writeNotes) = saveAndGenerateTraceReport trace
        printf "TraceReport: %A" traceReport

        // TODO
        traceReport.Events |> should haveLength 2
        let e = (getEvent traceReport (pNormal providerGuid 6))
        let s =  e.System
        s.Provider.Guid |> should equal (Some providerGuid)
        s.EventId |> should equal 6
        s.Level |> should equal (EventLevel.toInt EventLevel.Informational)
        s.Task |> should equal 4
        s.Opcode |> should equal 19
        s.Keywords |> should equal "0x8"

    [<Fact>]
    member x.``Generate single 64 bit system event ETL``() =
        let time = System.DateTime.UtcNow
        let event = new EtlSystemEvent()
        event.headerType <- EtlHeaderType.System64
        event.hookId <- 1us
        event.timestamp <- Util.toEtlAbsoluteTicks time
        event.payload <- [||]
        let trace = buildTrace true (time.AddHours(-1.0)) time [System event]
        let (traceReport, writeNotes) = saveAndGenerateTraceReport trace
        printf "TraceReport: %A" traceReport

        // TODO
        traceReport.Events |> should haveLength 2
        let e = (getEvent traceReport (pNormal providerGuid 6))
        let s =  e.System
        s.Provider.Guid |> should equal (Some providerGuid)
        s.EventId |> should equal 6
        s.Level |> should equal (EventLevel.toInt EventLevel.Informational)
        s.Task |> should equal 4
        s.Opcode |> should equal 19
        s.Keywords |> should equal "0x8"

    [<Fact>]
    member x.``Generate single 32 bit compact system event ETL``() =
        let time = System.DateTime.UtcNow
        let event = new EtlSystemEvent()
        event.headerType <- EtlHeaderType.Compact32
        event.hookId <- 1us
        event.timestamp <- Util.toEtlAbsoluteTicks time
        event.payload <- [||]
        let trace = buildTrace false (time.AddHours(-1.0)) time [System event]
        let (traceReport, writeNotes) = saveAndGenerateTraceReport trace
        printf "TraceReport: %A" traceReport

        traceReport.Events |> should haveLength 2
        let e = traceReport.Events |> Seq.where (fun e -> e.System.Opcode > 0) |> Seq.head
        let s =  e.System
        s.EventId |> should equal 0
        s.Level |> should equal (EventLevel.toInt EventLevel.Always)
        s.Task |> should equal 0
        s.Opcode |> should equal 1
        s.Keywords |> should equal "0x0"

    [<Fact>]
    member x.``Generate single 64 bit compact system event ETL``() =
        let time = System.DateTime.UtcNow
        let event = new EtlSystemEvent()
        event.headerType <- EtlHeaderType.Compact64
        event.hookId <- 1us
        event.timestamp <- Util.toEtlAbsoluteTicks time
        event.payload <- [||]
        let trace = buildTrace true (time.AddHours(-1.0)) time [System event]
        let (traceReport, writeNotes) = saveAndGenerateTraceReport trace
        printf "TraceReport: %A" traceReport

        // TODO
        traceReport.Events |> should haveLength 2
        let e = (getEvent traceReport (pNormal providerGuid 6))
        let s =  e.System
        s.Provider.Guid |> should equal (Some providerGuid)
        s.EventId |> should equal 6
        s.Level |> should equal (EventLevel.toInt EventLevel.Informational)
        s.Task |> should equal 4
        s.Opcode |> should equal 19
        s.Keywords |> should equal "0x8"

    [<Fact>]
    member x.``Generate single 32 bit perfinfo event ETL``() =
        let time = System.DateTime.UtcNow
        let event = new EtlSystemEvent()
        event.headerType <- EtlHeaderType.PerfInfo32
        event.hookId <- 1us
        event.timestamp <- Util.toEtlAbsoluteTicks time
        event.payload <- [||]
        let trace = buildTrace false (time.AddHours(-1.0)) time [System event]
        let (traceReport, writeNotes) = saveAndGenerateTraceReport trace
        printf "TraceReport: %A" traceReport

        // TODO
        traceReport.Events |> should haveLength 2
        let e = (getEvent traceReport (pNormal providerGuid 6))
        let s =  e.System
        s.Provider.Guid |> should equal (Some providerGuid)
        s.EventId |> should equal 6
        s.Level |> should equal (EventLevel.toInt EventLevel.Informational)
        s.Task |> should equal 4
        s.Opcode |> should equal 19
        s.Keywords |> should equal "0x8"

    [<Fact>]
    member x.``Generate single 64 bit perfinfo event ETL``() =
        let time = System.DateTime.UtcNow
        let event = new EtlSystemEvent()
        event.headerType <- EtlHeaderType.PerfInfo64
        event.hookId <- 1us
        event.timestamp <- Util.toEtlAbsoluteTicks time
        event.payload <- [||]
        let trace = buildTrace true (time.AddHours(-1.0)) time [System event]
        let (traceReport, writeNotes) = saveAndGenerateTraceReport trace
        printf "TraceReport: %A" traceReport

        // TODO
        traceReport.Events |> should haveLength 2
        let e = (getEvent traceReport (pNormal providerGuid 6))
        let s =  e.System
        s.Provider.Guid |> should equal (Some providerGuid)
        s.EventId |> should equal 6
        s.Level |> should equal (EventLevel.toInt EventLevel.Informational)
        s.Task |> should equal 4
        s.Opcode |> should equal 19
        s.Keywords |> should equal "0x8"
*)
    [<Fact>]
    member x.``Generate single 32 bit instance event ETL``() =
        let time = System.DateTime.UtcNow
        let event = new EtlInstanceGuidEvent()
        event.is64bit <- false
        let newGuid = System.Guid.NewGuid()
        event.guid <- newGuid
        event.instanceId <- 1u
        event.parentGuid <- System.Guid.Empty
        event.timestamp <- Util.toEtlAbsoluteTicks time
        event.payload <- [||]
        let trace = buildTrace false (time.AddHours(-1.0)) time [InstanceGuid event]
        let (traceReport, writeNotes) = saveAndGenerateTraceReport trace
        printf "TraceReport: %A" traceReport

        traceReport.Events |> should haveLength 2
        let e = (getEvent traceReport pInstance)
        let s =  e.System
        s.Provider.Guid |> should equal None
        s.EventId |> should equal 0
        s.Level |> should equal 0
        s.Task |> should equal 0
        s.Opcode |> should equal 2
        s.Keywords |> should equal "0x0"
        e.ExtendedTracingInfo.EventGuid |> should equal newGuid
        e.ExtendedTracingInfo.InstanceId |> should equal (Some 1)
        e.ExtendedTracingInfo.ParentInstanceId |> should equal (Some 0)

    [<Fact>]
    member x.``Generate single 64 bit instance event ETL``() =
        let time = System.DateTime.UtcNow
        let event = new EtlInstanceGuidEvent()
        event.is64bit <- true
        let newGuid = System.Guid.NewGuid()
        event.guid <- newGuid
        event.instanceId <- 1u
        event.parentGuid <- System.Guid.Empty
        event.timestamp <- Util.toEtlAbsoluteTicks time
        event.payload <- [||]
        let trace = buildTrace true (time.AddHours(-1.0)) time [InstanceGuid event]
        let (traceReport, writeNotes) = saveAndGenerateTraceReport trace
        printf "TraceReport: %A" traceReport

        traceReport.Events |> should haveLength 2
        let e = (getEvent traceReport pInstance)
        let s =  e.System
        s.Provider.Guid |> should equal None
        s.EventId |> should equal 0
        s.Level |> should equal 0
        s.Task |> should equal 0
        s.Opcode |> should equal 2
        s.Keywords |> should equal "0x0"
        e.ExtendedTracingInfo.EventGuid |> should equal newGuid
        e.ExtendedTracingInfo.InstanceId |> should equal (Some 1)
        e.ExtendedTracingInfo.ParentInstanceId |> should equal (Some 0)

    [<Fact>]
    member x.``Generate ETL with two buffers``() =
        let time = System.DateTime.UtcNow
        let event = new EtlSystemEvent()
        event.headerType <- EtlHeaderType.Compact32
        event.hookId <- 1us
        event.timestamp <- Util.toEtlAbsoluteTicks time
        event.payload <- [||]

        // Generate some events that will add up to more than 64k
        let allEvents = seq {
            for i in [1..100] do
                let e = event.Clone()
                e.timestamp <- e.timestamp + ((int64 i) * 10000L)
                yield (System e)
        }

        let trace = buildTrace false (time.AddHours(-1.0)) time allEvents
        let (traceReport, writeNotes) = saveAndGenerateTraceReport trace
        printf "TraceReport: %A" traceReport

        traceReport.Events |> should haveLength 101
        let firstEvent = traceReport.Events |> Seq.where (fun e -> e.System.Opcode > 0) |> Seq.head
        let s = firstEvent.System
        s.EventId |> should equal 0
        s.Level |> should equal 0
        s.Task |> should equal 0
        s.Opcode |> should equal 1
        s.Keywords |> should equal "0x0"
