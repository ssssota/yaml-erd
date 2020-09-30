open Util

match Parse.schemaFromFile @"./sample.yaml" |> Result.bind Validation.validate with
| Ok { Data = schema; Warnings = warnings } ->
    List.iter (fun warning -> Printf.eprintfn "%s" <| Util.warningToConsoleString warning) warnings;
    Print.schemaToFile @"./output.dot" schema
| Error errs ->
  List.iter (fun err -> Printf.eprintfn "%s" <| Util.errorToConsoleString err) errs
