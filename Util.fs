module Util

[<CustomEquality;NoComparison>]
type Position =
    { StartLine: int
      StartColumn: int
      EndLine: int
      EndColumn: int }

    override self.Equals other =
        match other with
        | :? Position as other ->
            self.StartLine = other.StartLine &&
            self.StartColumn = other.StartLine &&
            self.EndLine = other.EndLine &&
            self.EndColumn = other.EndColumn
        | _ -> false

    override self.GetHashCode () = hash (self)


type OkValue<'a, 'b> = { Data: 'a; Warnings: 'b [] }
type ExResult<'a, 'b, 'c> = Result<OkValue<'a, 'b>, 'c []>

module ExResult =
    let mkOk x = Ok { Data = x; Warnings = Array.empty }
    let mkError x = Error [| x |]

    let map f =
        function
        | Ok { Data = data; Warnings = warnings } -> Ok { Data = f data; Warnings = warnings }
        | Error err -> Error err

    let merge f r1 r2 =
        match r1, r2 with
        | Ok { Data = data1; Warnings = warnings1 }, Ok { Data = data2; Warnings = warnings2 } ->
            Ok
                { Data = f data1 data2
                  Warnings = Array.append warnings1 warnings2 }
        | Error err1, Error err2 -> Error <| Array.append err1 err2
        | _, Error err
        | Error err, _ -> Error err

module Option =
    let unwrap =
        function
        | None -> failwith "invalid unwrap"
        | Some x -> x

module List =

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
        | (name', x) :: xs -> if name = name' then Some x else lookup name xs

    let rec minus<'a when 'a: equality> (xs: 'a list) (ys: 'a list) =
        List.fold
            (fun acc y ->
                let _, acc = findPop ((=) y) acc
                acc)
            xs
            ys

    let appendOpt<'a> (xs: 'a list) (x: 'a option): 'a list =
        match x with
        | None -> xs
        | Some x -> x :: xs

    let rec tryNth<'a> (xs: 'a list) (i: int): 'a option =
        match xs with
        | [] -> None
        | x :: xs -> if i = 0 then Some x else tryNth xs (i - 1)

    let filterMap<'a, 'b> (f: 'a -> 'b option) (xs: 'a list): 'b list =
        let rec aux acc =
            function
            | [] -> acc
            | x :: xs ->
                match f x with
                | None -> aux acc xs
                | Some x -> aux (x :: acc) xs

        aux [] xs
