module OpenEtw.EtwGen.Program
open FParsec
open System.Text

open OpenEtw

let showUsage() =
    printf """EtwGen:

Generates code to implement Event Tracing for Windows (ETW) providers.
The input is in the form of a C++ header file using the macros defined in etw.h
Currently supported output languages are C++ and C#.

Usage:
EtwGen.exe [options] <input_file.h>

Options:
    -o OutDir: Specify an output directory. By default, the generated file will be written to the same directory as the input file
    -cpp_sd: Generate a self-describing C++ provider (emits its own manifest via special events when tracing is started / stopped)
    -cpp_man: Generate a manifested C++ provider (requires registration with wevtutil to decode events)
    -cs_eventsource: Generate a C# provider using System.Diagnostics.Tracing.EventSource
    -cs_legacy: Generate a C# provider using System.Diagnostics.Eventing.EventProvider
    -ns: Provide a namespace for C# providers (required if generating C# code)
    -nodejs: Generate a nodejs provider, implemented as a C++ module
"""

type CmdLineArgument =
    | InputFile of string
    | OutputDirectory of string
    | Namespace of string
    | Strategy of EtwStrategy
    | ShowUsage
    | Other

let pArgs =
    let ws = skipManySatisfy (isAnyOf " \t")
    let ws1 = skipMany1Satisfy (isAnyOf " \t")
    let pPath = 
        skipString "\"" >>. many1CharsTill anyChar (skipChar '\"')
        <|> 
        many1CharsTill anyChar (ws1 <|> eof)

    let pNamespace = many1Chars (noneOf [' '; '\t'])

    let pArg =
        choice
         [
            skipString "-ns" >>. ws >>. pNamespace |>> Namespace
            skipString "-o" >>. ws >>. pPath |>> OutputDirectory
            skipString "-cpp_sd" >>% Strategy (CppSelfDescribing)
            skipString "-cpp_man" >>% Strategy (CppManifested)
            skipString "-cs_legacy" >>% Strategy (CsLegacy)
            skipString "-cs_eventsource" >>% Strategy (CsEventSource)
            skipString "-nodejs" >>% Strategy (NodeJs)
            skipString "-help" >>% ShowUsage
            ws1 >>% Other
            pPath |>> InputFile
            newline >>% Other
         ]
    many pArg

type Arguments =
    {
        inputFile : string
        outputDir : string
        strategy : EtwStrategy
        dotNetNamespace : string option
    }

let buildFiles arguments provider =
    let headerName = System.IO.Path.GetFileName arguments.inputFile
    match arguments.strategy with
    | CppSelfDescribing -> 
        let cppName = System.IO.Path.ChangeExtension(headerName, "g.cpp")
        let implementationOptions = 
            { 
                precompiledHeader = None
                insertDebugLogging = false
                cppFilename = cppName 
                headerName = headerName
            }
        Public.generateCppSelfDescribing provider implementationOptions

    | CsLegacy -> 
        if(arguments.dotNetNamespace.IsNone) then failwith "C# providers require a namespace to be set using -ns" else
        let implementationOptions = 
            {
                csFilename = System.IO.Path.ChangeExtension(headerName, "cs")
                csNamespace = arguments.dotNetNamespace.Value
                decoratorFilename = Some (sprintf "%sDecorator.cs" (System.IO.Path.GetFileNameWithoutExtension headerName))
                manifestFilename = System.IO.Path.ChangeExtension(headerName, "man")
            }
        Public.generateCsLegacy provider implementationOptions
    | NodeJs ->
        let implementationOptions = 
            { 
                precompiledHeader = None
                insertDebugLogging = true
                cppFilename = System.IO.Path.ChangeExtension(headerName, "cpp")
                headerName = headerName
            }

        let implementationFiles = Public.generateCppSelfDescribing provider implementationOptions
        let jsOptions =
            { 
                NodeOptions.usePrecompiledHeader = true
                cppFilename = System.IO.Path.GetFileNameWithoutExtension headerName + ".Node.cpp"
                headerName = headerName
            }

        let wrapperFiles = Public.generateNodeWrapper provider jsOptions

        List.concat [ implementationFiles; wrapperFiles ]

    | x -> failwithf "The selected implementation strategy (%A) has not been implemented." x

let parseCommandLine argv =
    let combined = (String.concat " " argv)
    match runParserOnString pArgs () "" combined with
    | ParserResult.Success (results, _, _) ->
        let get f = results |> List.choose f |> List.tryHead
        let inputFile = get (function|InputFile x       -> Some x |_ -> None)
        let outputDir = get (function|OutputDirectory x -> Some x |_ -> None)
        let strategy  = get (function|Strategy x        -> Some x |_ -> None)
        let dotNetNamespace = get (function|Namespace x -> Some x |_ -> None)

        if Option.isNone inputFile then None else
        let fullPath = System.IO.Path.GetFullPath inputFile.Value

        {
            inputFile = fullPath
            outputDir = defaultArg outputDir (System.IO.Path.GetDirectoryName(fullPath))
            strategy = defaultArg strategy CppSelfDescribing
            dotNetNamespace = dotNetNamespace
        } |> Some

    | ParserResult.Failure (err,_,_) -> None

let generateEtwProvider arguments =
    let headerContent = System.IO.File.ReadAllText(arguments.inputFile) 

    printf "EtwGen: Parsing provider information in %s\n" arguments.inputFile
    match Public.parseHeader headerContent  with
    | Failure err -> eprintf "%s" err
    | Success provider ->
        let safetyCheck filename =
            if (not (System.IO.File.Exists filename)) then ()
            else
            let content = System.IO.File.ReadAllText filename
            if (not <| content.Contains("This file was generated by EtwGen.exe")) then
                failwithf "Tried to overwrite a non-autogenerated file %s" filename

        let implementation = buildFiles arguments provider

        implementation |> Seq.iter (fun (filename, content) ->
            safetyCheck filename // Ensure we only overwrite auto-generated files.
            let path = sprintf "%s\\%s" arguments.outputDir filename
            printf "EtwGen: Writing %s\n" path
            System.IO.File.WriteAllText(path, content, Encoding.UTF8))

[<EntryPoint>]
let main argv = 
    try
        match parseCommandLine argv with
        | None -> showUsage()
        | Some arguments -> generateEtwProvider arguments
        0
    with
    | e -> eprintf "Error: %A" e; 1
