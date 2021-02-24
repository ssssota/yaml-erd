open System
open System.Diagnostics
open Argu
open Util

type Format =
    | PNG
    | SVG
    | PDF

let private formatOfString (str: string) =
    match str.ToLower() with
    | "png" -> Ok PNG
    | "svg" -> Ok SVG
    | "pdf" -> Ok PDF
    | _ -> Error $"unknown format: {str}"

let private stringOfFormat =
    function
    | PNG -> "png"
    | SVG -> "svg"
    | PDF -> "pdf"

type private CommandLineParam =
    { Input: string
      Output: string
      Temp: string
      Format: Format
      Verbose: bool
      AdditionalDotParams: string }

let private compile args =
    let command = "dot"

    let pArgs =
        let format = stringOfFormat args.Format
        $"-T{format} {args.Temp} -o {args.Output} {args.AdditionalDotParams}"

    let procStartInfo =
        ProcessStartInfo(
            FileName = command,
            Arguments = pArgs,
            WorkingDirectory = ".",
            UseShellExecute = false,
            RedirectStandardOutput = true,
            RedirectStandardError = true
        )

    if args.Verbose then printfn $"[RUN] {command} {pArgs}"

    try
        use p = new Process(StartInfo = procStartInfo)
        do p.Start() |> ignore
        let output = p.StandardOutput.ReadToEnd()
        let error = p.StandardError.ReadToEnd()
        p.WaitForExit()
        let exitCode = p.ExitCode

        if args.Verbose then
            if output <> "" then printfn $"[STDOUT] \n{output}"
            if error <> "" then eprintfn $"[STDERR] \n{error}"
            if exitCode <> 0 then printfn $"[EXIT CODE] {exitCode}"
    with e ->
        eprintfn $"[PROCESS ERROR] {e.Message}"
        eprintfn "`dot` command not found...?"

type Arguments =
    | [<MainCommand; ExactlyOnce>] Input of filename: string
    | [<AltCommandLine("-t")>] Temp of filename: string
    | [<AltCommandLine("-o")>] Output of filename: string
    | [<AltCommandLine("-f")>] Format of string
    | Additional_Dot_Args of string
    | [<AltCommandLine("-v")>] Verbose

    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | Input _ -> "input yaml filename"
            | Temp _ -> "temporary dot filename"
            | Output _ -> "output filename"
            | Format _ -> "output format (\"png\", \"svg\" or \"pdf\")"
            | Additional_Dot_Args _ -> "additional dot params (e.g. -Nfontname=\"Calibri\")"
            | Verbose -> "show commands to run and verbose output"

[<EntryPoint>]
let main args =
    let parser =
        ArgumentParser.Create<Arguments>(programName = "yaml-erd")

    try
        let initArgs =
            { Input = ""
              Temp = ""
              Output = "./output.png"
              Format = PNG
              AdditionalDotParams = ""
              Verbose = false }

        let args =
            List.fold
                (fun args ->
                    function
                    | Input path -> Result.map (fun args -> { args with Input = path }) args
                    | Temp path -> Result.map (fun args -> { args with Temp = path }) args
                    | Output path -> Result.map (fun args -> { args with Output = path }) args
                    | Format format ->
                        Result.bind
                            (fun args ->
                                formatOfString format
                                |> Result.map (fun format -> { args with Format = format }))
                            args
                    | Additional_Dot_Args dotArg ->
                        Result.map
                            (fun args ->
                                { args with
                                      AdditionalDotParams = args.AdditionalDotParams + " " + dotArg })
                            args
                    | Verbose -> Result.map (fun args -> { args with Verbose = true }) args)
            <| Ok initArgs
            <| (parser.Parse args).GetAllResults()

        let args =
            match args with
            | Ok args -> args
            | Error err ->
                eprintfn $"invalid commandline: {err}"
                eprintfn $"{parser.PrintUsage()}"
                exit 1

        let temp =
            if args.Temp = "" then IO.Path.GetTempFileName() else args.Temp

        let args = { args with Temp = temp }

        match Parse.schemaFromFile args.Input with
        | Ok { Data = schema; Warnings = warnings } ->
            Print.schemaToFile temp schema
            compile args
            0
        | Error errs ->
            Array.iter (fun err -> eprintfn $"{err.ToString()}") errs
            1
    with e ->
        Printf.eprintfn $"{e.Message}"
        1
