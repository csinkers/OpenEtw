module OpenEtw.Tests.TraceRunner
open System.IO
open System.Text
open System.Diagnostics
open OpenEtw
open OpenEtw.Tests.GenerateHeader

let ensureDir dir = if (Directory.Exists dir |> not) then Directory.CreateDirectory dir |> ignore
let baseTestPath = @"C:\Tmp\EtwTests"

let environmentSetupBatchFiles = [
        @"C:\Program Files\Microsoft Visual Studio\2022\Enterprise\VC\Auxiliary\Build\vcvars32.bat"
        @"C:\Program Files\Microsoft Visual Studio\2022\Professional\VC\Auxiliary\Build\vcvars32.bat"
        @"C:\Program Files\Microsoft Visual Studio\2022\Community\VC\Auxiliary\Build\vcvars32.bat"
        @"C:\Program Files (x86)\Microsoft Visual Studio\2019\Enterprise\VC\Auxiliary\Build\vcvars32.bat"
        @"C:\Program Files (x86)\Microsoft Visual Studio\2019\Professional\VC\Auxiliary\Build\vcvars32.bat"
        @"C:\Program Files (x86)\Microsoft Visual Studio\2019\Community\VC\Auxiliary\Build\vcvars32.bat"
        @"C:\Program Files (x86)\Microsoft Visual Studio 14.0\VC\vcvars32.bat"
        @"C:\Program Files (x86)\Microsoft Visual Studio 9.0\VC\vcvars32.bat"
    ]

let environmentSetupBatchFile = 
    match List.tryFind System.IO.File.Exists environmentSetupBatchFiles with
    | Some file -> file
    | None -> failwithf "Could not find VS install"

type ProcessOptions =
    {
        name : string
        arguments : string
        workingDirectory : string
        log : System.IO.TextWriter
    }

let indentMultilineString str indent =
    String.collect (
        function
        | '\r' -> ""
        | '\n' -> "\n" + indent
        | x -> string x
    ) str

let runExe options =
    let startInfo = 
        ProcessStartInfo(
            options.name, 
            options.arguments, 
            WorkingDirectory = options.workingDirectory, 
            RedirectStandardOutput = true, 
            RedirectStandardError = true,
            UseShellExecute = false,
            CreateNoWindow = true
            )

    options.log.Write("{0:O}: Starting {1} {2}... ", System.DateTime.Now, options.name, options.arguments)
    let stopwatch = new Stopwatch()
    stopwatch.Start()
    use proc = Process.Start(startInfo)
    proc.WaitForExit()
    let stdout = proc.StandardOutput.ReadToEnd().Trim()
    let stderr = proc.StandardError.ReadToEnd().Trim()
    options.log.Write("finished after {0} ms with exit code {1}.\n", stopwatch.ElapsedMilliseconds, proc.ExitCode)

    if (not <| System.String.IsNullOrWhiteSpace stdout) then
        options.log.Write("\tSTDOUT:\n\t\t{0}\n\n", (indentMultilineString stdout "\t\t"))

    if (not <| System.String.IsNullOrWhiteSpace stderr) then
        options.log.Write("\tSTDERR:\n\t\t{0}\n\n", (indentMultilineString stderr "\t\t"))

    proc.ExitCode, stdout, stderr

let runTest testName provider events =
    let stopwatch = Stopwatch.StartNew()
    let testPath = Path.Combine(baseTestPath, testName)
    let originalManifest = Path.Combine(testPath, "Test.man")
    let parsedManifestPath = Path.Combine(testPath, "Parsed.man")
    let headerPath = Path.Combine(testPath, "Provider.h")
    let testHarnessPath = Path.Combine(testPath, "Main.cpp")
    let makefilePath = Path.Combine(testPath, "Makefile")
    let etwHeaderPath = Path.Combine(testPath, "etw.h")
    let etlPath = Path.Combine(testPath, "Test.etl")
    let buildBatchFilePath = Path.Combine(testPath, "build.bat")
    let logPath = Path.Combine(testPath, "Log.txt")
    let exePath = Path.Combine(testPath, "Test.exe")

    ensureDir baseTestPath
    ensureDir testPath

    let manifest = Public.generateManifest provider
    File.WriteAllText(originalManifest, manifest)

    let headerOptions = 
        {
            implicitEventNumbering = true
            elideHashedProviderGuid = false
            guidFormat = GuidFormat.HyphenatedDigitsWithBraces
        }

    let headerName = (provider.className + ".h")
    let headerContent = generateHeader provider headerOptions
    File.WriteAllText(headerPath, headerContent)

    match Public.parseHeader headerContent with
    | Failure error -> failwith error
    | Success parsedProvider ->
        // TODO: Assert parsed provider = generated provider
        let manifest = Public.generateManifest parsedProvider
        File.WriteAllText(parsedManifestPath, manifest)

        if (parsedProvider <> provider) then
            printfn "Provider parsed from header does not match test provider." // Warn

        let cppFilename = System.IO.Path.ChangeExtension(headerName, "cpp")
        let implementationOptions = 
            {
                insertDebugLogging = true
                cppFilename = cppFilename
                headerName = headerName 
                etwGenComment = Util.buildEtwGenComment headerPath
            }

        let implementation = Public.generateCppSelfDescribing parsedProvider implementationOptions
        implementation |> Seq.iter (fun (filename, content) ->
            let path = Path.Combine(testPath, filename)
            File.WriteAllText(path, content, Encoding.UTF8))

        let bitness = false // Placeholder
        let cppTestHarness = CppTestHarness.build provider events bitness
        File.WriteAllText(testHarnessPath, cppTestHarness)

        use etwHeaderStream = System.Reflection.Assembly.GetExecutingAssembly().GetManifestResourceStream("EtwGen.Tests.etw.h")
        use reader = new StreamReader(etwHeaderStream)
        let etwHeader = reader.ReadToEnd()
        File.WriteAllText(etwHeaderPath, etwHeader)

        let makefile = 
            $"""
CPPFLAGS=$(CPPFLAGS)
all: Test.exe
Test.exe: Main.obj Provider.obj
"""         + "\t" + "link /OUT:Test.exe $** advapi32.lib"

        File.WriteAllText(makefilePath, makefile)

        let buildBatchFileContent =
            $"""
@echo off
call "{environmentSetupBatchFile}"
nmake
"""                 
        File.WriteAllText(buildBatchFilePath, buildBatchFileContent)

        use fs = File.Open(logPath, FileMode.Create)
        use tw = new StreamWriter(fs)
        let options =
            {
                name = ""
                arguments = ""
                workingDirectory = testPath
                log = tw
            }

        // Run make
        let makeExitCode, makeStdOut, makeStdErr = runExe { options with name = @"cmd"; arguments = $"/c \"{buildBatchFilePath}\"" }
        if (makeExitCode <> 0) then
            failwithf "Make failed with code %d:\nSTDOUT:\n%s\nSTDERR:\n%s" makeExitCode makeStdOut makeStdErr

        // Check result, throw w/ output if errored.

        // Emit the events into a new session
        runExe { options with name = "xperf"; arguments = $"-start {testName} -on {provider.guid} -f session.etl" } |> ignore
        runExe { options with name = exePath } |> ignore
        runExe { options with name = "xperf"; arguments = $"-stop {testName}" } |> ignore
        runExe { options with name = "tracerpt"; arguments = "-of xml -o report.xml -y session.etl" } |> ignore

        // Load ETL file
        // Extract provider
        // Compare schemas
        // Extract events
        // Compare events
        tw.Write("Test finished after {0} ms\n", stopwatch.ElapsedMilliseconds)
