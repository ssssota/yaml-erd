open Util

match Parse.schemaFromFile @"./sample_.yaml" |> Result.bind PostProc.postProc (* |> Result.bind Validation.validate *) with
| Ok { Data = schema; Warnings = warnings } ->
    List.iter (fun warning -> Printf.eprintfn "%s" <| warningToConsoleString warning) warnings;
    Print.schemaToFile @"./output.dot" schema
| Error errs ->
  List.iter (fun err -> Printf.eprintfn "%s" <| errorToConsoleString err) errs
