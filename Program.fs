open Util
open Argu

type Arguments =
  | [<MainCommand; ExactlyOnce>]Input of filename:string
  | [<AltCommandLine("-o")>]Output of filename:string

  interface IArgParserTemplate with
    member s.Usage =
        match s with
        | Input _ -> "input yaml filename"
        | Output _ -> "output dot filename"

[<EntryPoint>]
let main args =
  let parser = ArgumentParser.Create<Arguments>(programName = "yaml-erd")
  try
    let input, output = List.fold (fun (input, output) -> function
      | Input path -> path, output
      | Output path -> input, path) ("", "./output.dot") <| (parser.Parse args).GetAllResults()

    match Parse.schemaFromFile input |> Util.Result.bind Validation.validate with
    | Ok { Data = schema; Warnings = warnings } ->
      List.iter (fun warning -> Printf.eprintfn "%s" <| warning.ToString()) warnings;
      Print.schemaToFile output schema
      0
    | Error errs ->
      List.iter (fun err -> Printf.eprintfn "%s" <| err.ToString()) errs
      -1
  with
  | e ->
    Printf.eprintfn "%A" e
    -1