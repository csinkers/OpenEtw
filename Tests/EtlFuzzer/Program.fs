module CreateFuzzEtl.Program
open System
open System.IO
open System.Runtime.InteropServices
open OpenEtw

let providerGuid = Guid.Parse "F8FDE2D4-002C-4122-863D-8A21E242D321"
let etlEpochTicks = (new DateTime(1601, 1, 1)).Ticks
let threadId = Util.GetCurrentThreadId() // TODO
let procId =
    use curProcess = System.Diagnostics.Process.GetCurrentProcess()
    (uint32 curProcess.Id) // TODO

[<DllImport("kernel32.dll")>]
extern uint64 GetTickCount64()

let generateEvents bootTime (startTime:DateTime) (endTime:DateTime) points =
    seq {
        let baseInterval = (endTime - startTime).TotalSeconds / (float points)
        for i in [0..points-1] do
            let event =
                let payload =
                    [
                        (BitConverter.GetBytes (uint32 i))
                    ] |> Seq.concat |> Array.ofSeq

                EtlEvent(
                    threadId = threadId,
                    processId = procId,
                    timestamp = Util.toEtlBootTicks bootTime (startTime.AddSeconds(baseInterval * (float i))),
                    provider = providerGuid,
                    eventId = 0x1us,
                    version = 1uy,
                    opcode = 0x1uy,
                    taskId = 0x1us,
                    payload = payload
                )

            yield event
    }

[<EntryPoint>]
[<STAThread>]
let main argv =
    let outputEtl = @"fuzz.etl"
    let outputXml = @"fuzz.xml"

    let eventCount = 5
    let msSinceBoot = GetTickCount64() |> float
    let bootTime = DateTime.Now.AddMilliseconds(-msSinceBoot)
    let startTime = DateTime.Now
    let endTime = DateTime.Now.AddSeconds(10.0)

    let events = 
        generateEvents bootTime startTime endTime eventCount 
        |> Seq.map (fun e -> (Event e, 0uy)) // Add dummy cpuId

    let bufferSize = 0x10000
    let trace =
        new EtlTrace(
            threadId = threadId,
            processId = procId,
            sessionName = "Fuzz",
            fileName = outputEtl,
            bootTime = bootTime,
            startTime = startTime,
            endTime   = Some endTime,
            bufferSize = bufferSize,
            buffers = EtlBuffer.BuildBuffers bufferSize events
        )

    let shortMutators =
        [
            "unk00s",  (fun (t:EtlTrace) -> float t.unk00s), fun (t:EtlTrace) (v:float) -> (t.unk00s <- int16 v; t)
            "unk02s",  (fun (t:EtlTrace) -> float t.unk02s), fun (t:EtlTrace) (v:float) -> (t.unk02s <- int16 v; t)
            "unk34s",  (fun (t:EtlTrace) -> float t.unk34s), fun (t:EtlTrace) (v:float) -> (t.unk34s <- int16 v; t)
        ]

    let intMutators =
        [
            "unk04i",  (fun (t:EtlTrace) -> float t.unk04i),  fun (t:EtlTrace) (v:float) -> (t.unk04i <- int32 v; t)
            "unk08i",  (fun (t:EtlTrace) -> float t.unk08i),  fun (t:EtlTrace) (v:float) -> (t.unk08i <- int32 v; t)
            "unk0ci",  (fun (t:EtlTrace) -> float t.unk0ci),  fun (t:EtlTrace) (v:float) -> (t.unk0ci <- int32 v; t)
            "unk30i",  (fun (t:EtlTrace) -> float t.unk30i),  fun (t:EtlTrace) (v:float) -> (t.unk30i <- int32 v; t)
//            "unk6ci",  (fun (t:EtlTrace) -> float t.unk6ci),  fun (t:EtlTrace) (v:float) -> (t.unk6ci <- int32 v; t)
            "unkb0i",  (fun (t:EtlTrace) -> float t.unkb0i),  fun (t:EtlTrace) (v:float) -> (t.unkb0i <- int32 v; t)
//            "unk15ci", (fun (t:EtlTrace) -> float t.unk15ci), fun (t:EtlTrace) (v:float) -> (t.unk15ci <- int32 v; t)
        ]

    let longMutators =
        [
            "unk10l",  (fun (t:EtlTrace) -> float t.unk10l),  fun (t:EtlTrace) (v:float) -> (t.unk10l <- int64 v; t)
            "unk18l",  (fun (t:EtlTrace) -> float t.unk18l),  fun (t:EtlTrace) (v:float) -> (t.unk18l <- int64 v; t)
            "unk20l",  (fun (t:EtlTrace) -> float t.unk20l),  fun (t:EtlTrace) (v:float) -> (t.unk20l <- int64 v; t)
            "unk38l",  (fun (t:EtlTrace) -> float t.unk38l),  fun (t:EtlTrace) (v:float) -> (t.unk38l <- int64 v; t)
            "unk40l",  (fun (t:EtlTrace) -> float t.unk40l),  fun (t:EtlTrace) (v:float) -> (t.unk40l <- int64 v; t)
        ]

    let tryDeleteFile f =
        try System.IO.File.Delete(f)
        with | :? System.UnauthorizedAccessException as e -> ()
             | :? System.IO.IOException              as e -> ()

    let updateFunc (t:EtlTrace) =
        (
            use fs = File.OpenWrite(outputEtl)
            use bw = new BinaryWriter(fs)
            let gw = new Util.GenericBinaryWriter(bw)
            t.Serialize gw
        )

        tryDeleteFile outputXml
        let retVal, std, err = CreateFuzzEtl.Util.runExe "tracerpt" (sprintf "%s -o %s -of XML -y" outputEtl outputXml) (TimeSpan.FromSeconds(10.0))
        if (File.Exists outputXml) then File.ReadAllText(outputXml).Replace("\t", "  ")
        else sprintf "STDOUT:\n%s\n\nSTDERR:\n%s\n" std err

    let gui = new CreateFuzzEtl.Gui.FuzzerGui<EtlTrace>(updateFunc, trace)
    shortMutators |> Seq.iter (fun (n, getter, setter) -> gui.AddParam(n, getter, setter, -32768.0, 32767.0))
    intMutators   |> Seq.iter (fun (n, getter, setter) -> gui.AddParam(n, getter, setter, -(2.0**31.0), 2.0**31.0 - 1.0))
    longMutators  |> Seq.iter (fun (n, getter, setter) -> gui.AddParam(n, getter, setter, -(2.0**63.0), 2.0**63.0 - 1.0))

    gui.Window.ShowDialog() |> ignore
    0
