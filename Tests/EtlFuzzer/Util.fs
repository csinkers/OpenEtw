module CreateFuzzEtl.Util
open System.Diagnostics

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
