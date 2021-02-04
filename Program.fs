open System
open System.Diagnostics
open Util
open Argu

let private compile input output =
  let args = String.Format("-Tpng {0} -o {1}", input, output)
  let procStartInfo = ProcessStartInfo(FileName = "dot", Arguments = args, WorkingDirectory = ".", UseShellExecute = false)
  let ps = new Process(StartInfo = procStartInfo)
  ps.Start() |> ignore
  ps.WaitForExit()

type Arguments =
  | [<MainCommand; ExactlyOnce>]Input of filename:string
  | [<AltCommandLine("-t")>]Temp of filename:string
  | [<AltCommandLine("-o")>]Output of filename:string

  interface IArgParserTemplate with
    member s.Usage =
        match s with
        | Input _ -> "input yaml filename"
        | Temp _ -> "temporal dot filename"
        | Output _ -> "output png filename"

[<EntryPoint>]
let main args =
  let parser = ArgumentParser.Create<Arguments>(programName = "yaml-erd")
  try
    let input, temp, output = List.fold (fun (input, temp, output) -> function
      | Input path -> path, temp, output
      | Temp path -> input, path, output
      | Output path -> input, temp, path) ("", "", "./output.png") <| (parser.Parse args).GetAllResults()

    let temp = if temp = "" then System.IO.Path.GetTempFileName() else temp

    match Parse.schemaFromFile input |> Util.Result.bind Validation.validate with
    | Ok { Data = schema; Warnings = warnings } ->
      List.iter (fun warning -> Printf.eprintfn "%s" <| warning.ToString()) warnings;
      Print.schemaToFile temp schema
      compile temp output
      0
    | Error errs ->
      List.iter (fun err -> Printf.eprintfn "%s" <| err.ToString()) errs
      -1
  with
  | e ->
    Printf.eprintfn "%A" e
    -1