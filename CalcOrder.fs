module CalcOrder

open System.Collections.Generic
open Util
open Schema

type Path = string list

let private solveShortestPath (state: Dictionary<string, string list option>) (graph: (string * string list) list) init =
    let rec aux = function
        | [] -> ()
        | x :: xs ->
            match List.lookup x graph with
            | None -> aux xs
            | Some dists ->
                let current = Option.unwrap state.[x]
                let xs: string list =
                    List.fold
                        (fun (acc: string list) (dist: string) ->
                            if state.ContainsKey dist then
                                match state.[dist] with
                                | None ->
                                    state.[dist] <- Some (x :: current)
                                    dist :: acc
                                | Some path ->
                                    if not <| List.contains dist current && List.length current + 1 > List.length path then
                                        state.[dist] <- Some (x :: current)
                                        dist :: acc
                                    else
                                        acc
                            else
                                acc
                        )
                        xs
                        dists
                aux xs
    aux [init]


let private longestPath (entityNames: string list) (graph: (string * string list) list): Path =
    List.fold (fun (acc: string list) entity ->
        let state = new Dictionary<string, string list option>()
        List.iter (fun name -> state.Add(name, None)) entityNames
        state.[entity] <- Some []
        solveShortestPath state graph entity
        List.iter (fun key ->
            match state.[key] with
            | None -> ()
            | Some path -> state.[key] <- Some (key :: path)) (List.ofSeq state.Keys)
        let max = List.maxOf (function
            | None -> 0
            | Some xs -> List.length xs) None (List.ofSeq state.Values)
        match max with
        | None -> acc
        | Some max when List.length acc > List.length max -> acc
        | Some max -> max
    ) [] entityNames

let private makeColumns (schema: Schema.T): string list list =
    let graph: (string * string list) list =
        List.fold (fun (acc: (string * string list) list) (entity: Entity) ->
            List.fold (fun (acc: (string * string list) list) (relation: Relation) ->
                match List.findPop (fun (x, _) -> x = entity.Name) acc with
                | None, acc -> (entity.Name, [fst relation.Dist]) :: acc
                | Some (_, dists), acc ->
                    match List.findPop ((=) (fst relation.Dist)) dists with
                    | Some _, dists | None, dists -> (entity.Name, fst relation.Dist :: dists) :: acc)
                acc
                entity.Relations)
            []
            schema
    let rec aux acc entityNames =
        let longest = longestPath entityNames graph
        if List.isEmpty longest then acc
        else aux (List.rev longest :: acc) <| List.minus entityNames longest
    aux [] <| List.map (fun entity -> entity.Name) schema |> List.rev


let private solveShortestRowPath (state: int list option array) (graph: int list array) init =
    let rec aux = function
        | [] -> ()
        | x :: xs ->
            let dists = graph.[x]
            let current = Option.unwrap state.[x]
            let xs: int list =
                List.fold
                    (fun (acc: int list) (dist: int) ->
                        match state.[dist] with
                        | None ->
                            state.[dist] <- Some (x :: current)
                            dist :: acc
                        | Some path ->
                            if not <| List.contains dist current && List.length current + 1 > List.length path then
                                state.[dist] <- Some (x :: current)
                                dist :: acc
                            else
                                acc
                    )
                    xs
                    dists
            aux xs
    aux [init]

let private longestRowPath (entityIndices: int list) (graph: int list array): int list =
    List.fold (fun (acc: int list) entity ->
        let state: int list option array = Array.map (fun _ -> None) graph
        state.[entity] <- Some []
        solveShortestRowPath state graph entity
        Array.iteri (fun idx path ->
            match path with
            | None -> ()
            | Some path -> state.[idx] <- Some path) state
        let max = List.maxOf (function
            | None -> 0
            | Some xs -> List.length xs) None (List.ofSeq state)
        match max with
        | None -> acc
        | Some max when List.length acc > List.length max -> acc
        | Some max -> max
    ) [] entityIndices

let private makeRows (schema: Schema.T) (rows: string list array): string list list =
    let graph: int list array = Array.map (fun _ -> []) rows
    let graph: int list array =
        Array.fold (fun (graph, srcIdx) (row: string list) ->
            List.fold (fun (graph: int list array) (entityName: string) ->
                let entity = List.find (fun entity -> entity.Name = entityName) schema
                List.fold (fun graph relation ->
                    let distIdx = Array.findIndex (List.contains (fst relation.Dist)) rows
                    graph.[srcIdx] <- distIdx :: graph.[srcIdx]
                    graph
                ) graph entity.Relations
            ) graph row, (srcIdx+1)
        ) (graph, 0) rows |> fst
    let rec aux (acc: int list list) (entityIndices: int list) =
        let longest: int list = longestRowPath entityIndices graph
        if List.isEmpty longest then acc
        else aux (List.rev longest :: acc) <| List.minus entityIndices longest
    let indices = aux [] <| List.ofSeq (seq { 0 .. (Array.length rows) - 1})
    List.fold (fun acc indices ->
        List.fold (fun acc idx ->
            rows.[idx] :: acc
        ) acc indices
    ) [] indices


let calcOrder (schema: Schema.T): string list list =
    makeColumns schema |> Array.ofList |>  makeRows schema
