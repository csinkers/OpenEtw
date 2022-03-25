open System
open System.IO
open OpenEtw
open SerdesNet

let printUsage() =
    printfn """Usage: FilterEtl
    -i <InputFile>
    -o <OutputFile>
    -min <time in seconds, e.g. 123.02>                     : Discard all events with a relative timestamp prior to the given value
    -max <time in seconds, e.g. 123.02>                     : Discard all events with a relative timestamp after the the given value
    -min <ISO8601 timestamp, e.g. 2017-02-13T15:13:22.138Z> : Discard all events with an absolute timestamp prior to the given time
    -max <ISO8601 timestamp, e.g. 2017-02-13T15:13:22.138Z> : Discard all events with an absolute timestamp after the given time
    +provider <provider guid or name>                       : Discard all events except the given provider, multiple -inc parameters can be supplied.
    -provider <provider guid or name>                       : Discard all events for the given provider
    +pid <process id>                                       : Discard all events except those pertaining to the given process
    +tid <thread id>                                        : Discard all events except those pertaining to the given thread
    +process <process name>                                 : Discard all events except those pertaining to the given process name
    -pid <process id>                                       : Discard all events pertaining to the given process
    -tid <thread id>                                        : Discard all events pertaining to the given thread
    -process <process name>                                 : Discard all events pertaining to the given process name

    By default, process and module events are never pruned.
    Firstly, all positive filters are combined, and any events that don't match are discarded.
    Secondly, any negative filters are applied and any events that match are discarded."""

[<EntryPoint>]
let main argv = 
    let inputFilename = @"C:\tmp\ScDiag.etl"
    let outputFilename = @"C:\tmp\filtered.etl"

    use stream = File.OpenRead(inputFilename)
    use binaryReader = new BinaryReader(stream)
    use reader = Util.buildReader binaryReader
    let trace = EtlTrace.Deserialize reader

    let events = 
        trace.buffers
        |> Seq.map (fun b -> b.events |> Seq.map (fun e -> (e, b.cpuId)))
        |> Seq.concat

    let cutoffTicks = Util.toEtlBootTicks trace.bootTime (new DateTime(2017, 08, 18, 20, 33, 0))
    let targetGuid = Guid.Parse("")

    let mutable total = 0
    let filteredEvents = 
        events
        |> Seq.filter (
            function
            | Event e, _        ->
//                if (e.provider <> targetGuid (*|| e.processId <> 4060u*)) then false else
                if (e.provider = targetGuid) then false else
                total <- total + 1
                total <= 1000 // e.timestamp >= cutoffTicks
            | System e, _       ->
                total <- total + 1
                total <= 1000 // e.timestamp >= cutoffTicks
            | TraceEvent e, _   -> 
                total <- total + 1
                total <= 1000 // e.timestamp >= cutoffTicks
            | InstanceGuid e, _ ->
                total <- total + 1
                total <= 1000 // e.timestamp >= cutoffTicks
            | _ -> true)

    let outputBufferSize = 
        //64 * 1024  // TraceEvent doesn't want to read files w/ 64k buffers
        trace.bufferSize

    let filteredTrace = trace.Clone()
    filteredTrace.fileName   <- outputFilename
    filteredTrace.bufferSize <- outputBufferSize
    filteredTrace.buffers    <- EtlBuffer.BuildBuffers outputBufferSize filteredEvents

    use fs = File.Open(outputFilename, FileMode.Create)
    use binaryWriter = new BinaryWriter(fs)
    use writer = Util.buildWriter binaryWriter
    filteredTrace.Serialize writer

    0
