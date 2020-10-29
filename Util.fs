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

module Option =
    let unwrap = function
    | None -> failwith "invalid unwrap"
    | Some x -> x

module List =
    let maxOf<'a> (f: 'a -> int) (d: 'a) (xs: 'a list): 'a =
        List.fold (fun acc x ->
            let v = f x
            if v > fst acc then v, x
            else acc
        ) (f d, d) xs |> snd

    let rec mem<'a when 'a: equality> (x: 'a) =
        function
        | [] -> false
        | y :: ys -> if x = y then true else mem x ys

    let rec findPop<'a> (pred: 'a -> bool) (xs: 'a list): 'a option * 'a list =
        match xs with
        | [] -> None, []
        | x :: xs when pred x -> Some x, xs
        | x :: xs ->
            match findPop pred xs with
            | None, xs -> None, x :: xs
            | Some x_, xs -> Some x_, x :: xs

    let rec lookup (name: string) (xs: (string * 'a) list): 'a option =
        match xs with
        | [] -> None
        | (name', x) :: xs ->
            if name = name' then Some x
            else lookup name xs

    let rec minus<'a when 'a: equality> (xs: 'a list) (ys: 'a list) =
        List.fold (fun acc y ->
            let _, acc = findPop ((=) y) acc
            acc) xs ys

    let appendOpt<'a> (xs: 'a list) (x: 'a option): 'a list =
        match x with
        | None -> xs
        | Some x -> x :: xs
