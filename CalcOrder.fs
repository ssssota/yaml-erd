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
            | Option.None -> aux xs
            | Some dists ->
                let current = Option.unwrap state.[x]
                let xs: string list =
                    List.fold
                        (fun (acc: string list) (dist: string) ->
                            if state.ContainsKey dist then
                                match state.[dist] with
                                | Option.None ->
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
        List.iter (fun name -> state.Add(name, Option.None)) entityNames
        state.[entity] <- Some []
        solveShortestPath state graph entity
        List.iter (fun key ->
            match state.[key] with
            | Option.None -> ()
            | Some path -> state.[key] <- Some (key :: path)) (List.ofSeq state.Keys)
        let max = List.maxOf (function
            | Option.None -> 0
            | Some xs -> List.length xs) Option.None (List.ofSeq state.Values)
        match max with
        | Option.None -> acc
        | Some max when List.length acc > List.length max -> acc
        | Some max -> max
    ) [] entityNames

let calcOrder (schema: Schema.T): string list list =
    let graph: (string * string list) list =
        List.fold (fun (acc: (string * string list) list) (entity: Entity) ->
            List.fold (fun (acc: (string * string list) list) (relation: Relation) ->
                match List.findPop (fun (x, _) -> x = entity.Name) acc with
                | Option.None, acc -> (entity.Name, [fst relation.Dist]) :: acc
                | Some (_, dists), acc ->
                    match List.findPop ((=) (fst relation.Dist)) dists with
                    | Some _, dists | Option.None, dists -> (entity.Name, fst relation.Dist :: dists) :: acc)
                acc
                entity.Relations)
            []
            schema
    let rec aux acc entityNames =
        let longest = longestPath entityNames graph
        if List.isEmpty longest then acc
        else aux (List.rev longest :: acc) <| List.minus entityNames longest
    aux [] <| List.map (fun entity -> entity.Name) schema |> List.rev

