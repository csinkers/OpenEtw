open System.IO
open OpenEtw

[<EntryPoint>]
let main argv = 
    match argv.Length with
    | 1 ->
        use stream = File.OpenRead(argv.[0])
        use br = new BinaryReader(stream)
        let reader = new Util.GenericBinaryReader(br)
        let trace = EtlTrace.Deserialize reader

        let writer = new Util.AnnotatedFormatWriter(System.Console.Out)

        try
            trace.Serialize writer
        with | e -> printf "Error: %A\r\n" e

//        printf "%s\r\n" <| sw.ToString()
//        Console.ReadLine() |> ignore
    | _ -> printf "Usage: DumpEtlHeaders <EtlFile>\r\n"
        
    0
