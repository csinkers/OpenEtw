open System.IO
open OpenEtw
open SerdesNet

[<EntryPoint>]
let main argv = 
    match argv.Length with
    | 1 ->
        use stream = File.OpenRead(argv.[0])
        use br = new BinaryReader(stream)
        use reader = Util.buildReader(br)
        let trace = EtlTrace.Deserialize reader

        use ms = new MemoryStream()
        use bw = new BinaryWriter(ms)
        use innerWriter = Util.buildWriter(bw)
        use writer = new AnnotationProxySerdes(innerWriter, System.Console.Out, fun s -> System.Text.Encoding.UTF8.GetBytes(s))

        try
            trace.Serialize writer
        with | e -> printf "Error: %A\r\n" e

//        printf "%s\r\n" <| sw.ToString()
//        Console.ReadLine() |> ignore
    | _ -> printf "Usage: DumpEtlHeaders <EtlFile>\r\n"
        
    0
