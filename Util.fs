module Util

open System

type Warning =
    { StartLine: int
      StartColumn: int
      EndLine: int
      EndColumn: int
      Message: string }

let warningToConsoleString w =
    String.Format
        ("\x1b[33m warning[{0}:{1}-{2}:{3}]: \x1b[0m {4}", w.StartLine, w.StartColumn, w.EndLine, w.EndColumn, w.Message)

type ErrorValue =
    { StartLine: int
      StartColumn: int
      EndLine: int
      EndColumn: int
      Message: string }

let errorToConsoleString e =
    String.Format
        ("\x1b[31m error[{0}:{1}-{2}:{3}]: \x1b[0m {4}", e.StartLine, e.StartColumn, e.EndLine, e.EndColumn, e.Message)

type OkValue<'a> = { Data: 'a; Warnings: Warning list }
type Result<'a> = Result<OkValue<'a>, ErrorValue list>

module Result =
    let mkOk x = Ok { Data = x; Warnings = [] }

    let bind f =
        function
        | Ok { Data = data; Warnings = warnings1 } ->
            match f data with
            | Ok { Data = data; Warnings = warnings2 } ->
                Ok
                    { Data = data
                      Warnings = warnings1 @ warnings2 }
            | Error err -> Error err
        | Error err -> Error err

    let mapOk f =
        function
        | Ok { Data = data; Warnings = warnings } -> Ok { Data = f data; Warnings = warnings }
        | Error err -> Error err

    let merge f r1 r2 =
        match r1, r2 with
        | Ok { Data = data1; Warnings = warnings1 }, Ok { Data = data2; Warnings = warnings2 } ->
            Ok
                { Data = f data1 data2
                  Warnings = warnings1 @ warnings2 }
        | Error err1, Error err2 -> Error <| err1 @ err2
        | _, Error err
        | Error err, _ -> Error err

module List =
    let rec findPop<'a> (pred: 'a -> bool) (xs: 'a list): 'a option * 'a list =
        match xs with
        | [] -> None, []
        | x :: xs when pred x -> Some x, xs
        | x :: xs ->
            match findPop pred xs with
            | None, xs -> None, x :: xs
            | Some x_, xs -> Some x_, x :: xs
