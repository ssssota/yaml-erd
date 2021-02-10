open System
open System.Diagnostics
open Argu

type Format =
    | PNG
    | SVG
    | PDF

let private formatOfString (str: string) =
    match str.ToLower() with
    | "png" -> Ok PNG
    | "svg" -> Ok SVG
    | "pdf" -> Ok PDF
    | _ -> Error <| String.Format("unknown format: {0}", str)

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
      AdditionalDotParams: string
      EntitySets: string [] list }

let private compile args =
    let command = "dot"

    let pArgs =
        String.Format(
            "-T{0} {1} -o {2} {3}",
            stringOfFormat args.Format,
            args.Temp,
            args.Output,
            args.AdditionalDotParams
        )

    let procStartInfo =
        ProcessStartInfo(
            FileName = command,
            Arguments = pArgs,
            WorkingDirectory = ".",
            UseShellExecute = false,
            RedirectStandardOutput = true,
            RedirectStandardError = true
        )

    if args.Verbose then printfn "[RUN] %s %s" command pArgs

    try
        use p = new Process(StartInfo = procStartInfo)
        do p.Start() |> ignore
        let output = p.StandardOutput.ReadToEnd()
        let error = p.StandardError.ReadToEnd()
        p.WaitForExit()
        let exitCode = p.ExitCode

        if args.Verbose then
            if output <> "" then printfn "[STDOUT] \n%s" output
            if error <> "" then eprintfn "[STDERR] \n%s" error
            if exitCode <> 0 then printfn "[EXIT CODE] %d" exitCode
    with e ->
        eprintfn "[PROCESS ERROR] %s" e.Message
        eprintfn "`dot` command not found...?"

type Arguments =
    | [<MainCommand; ExactlyOnce>] Input of filename: string
    | [<AltCommandLine("-t")>] Temp of filename: string
    | [<AltCommandLine("-o")>] Output of filename: string
    | [<AltCommandLine("-f")>] Format of string
    | Additional_Dot_Args of string
    | [<AltCommandLine("-es")>] Entity_Set of string
    | [<AltCommandLine("-v")>] Verbose

    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | Input _ -> "input yaml filename"
            | Temp _ -> "temporary dot filename"
            | Output _ -> "output filename"
            | Format _ -> "output format (\"png\", \"svg\" or \"pdf\")"
            | Additional_Dot_Args _ -> "additional dot params (e.g. -Nfontname=\"Calibri\")"
            | Entity_Set _ -> "entity names which are in a row (delimited with ':', e.g. \"entity1:entity2:entity3\")"
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
              EntitySets = []
              Verbose = false }

        let args =
            List.fold
                (fun args ->
                    function
                    | Input path ->
                        args
                        |> Result.map (fun args -> { args with Input = path })
                    | Temp path ->
                        args
                        |> Result.map (fun args -> { args with Temp = path })
                    | Output path ->
                        args
                        |> Result.map (fun args -> { args with Output = path })
                    | Format format ->
                        args
                        |> Result.bind
                            (fun args ->
                                formatOfString format
                                |> Result.map (fun format -> { args with Format = format }))
                    | Additional_Dot_Args dotArg ->
                        args
                        |> Result.map
                            (fun args ->
                                { args with
                                      AdditionalDotParams = args.AdditionalDotParams + " " + dotArg })
                    | Entity_Set entitySet ->
                        args
                        |> Result.map
                            (fun args ->
                                { args with
                                      EntitySets = (entitySet.Split ':') :: args.EntitySets })
                    | Verbose ->
                        args
                        |> Result.map (fun args -> { args with Verbose = true }))
            <| Ok initArgs
            <| (parser.Parse args).GetAllResults()

        let args =
            match args with
            | Ok args -> args
            | Error err ->
                eprintfn "invalid commandline: %s" err
                eprintfn "%s" <| parser.PrintUsage()
                exit 1

        let temp =
            if args.Temp = "" then System.IO.Path.GetTempFileName() else args.Temp

        let args = { args with Temp = temp }

        match Parse.schemaFromFile args.Input
              |> Util.Result.bind Validation.validate with
        | Ok { Data = schema; Warnings = warnings } ->
            List.iter (fun warning -> Printf.eprintfn "%s" <| warning.ToString()) warnings
            Print.schemaToFile temp schema args.EntitySets
            compile args
            0
        | Error errs ->
            List.iter (fun err -> Printf.eprintfn "%s" <| err.ToString()) errs
            1
    with e ->
        Printf.eprintfn "%s" e.Message
        1
