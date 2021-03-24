module Graph

open System.Collections.Generic
open Util

type T<'a> = Dictionary<'a, 'a list>
type Path<'a> = 'a list

type private State<'a> = Dictionary<'a, Path<'a> option>

let empty<'a when 'a: equality> = new T<'a>()

let add<'a when 'a: equality> (x, y) (graph: T<'a>) =
    if not <| Seq.contains x graph.Keys then
        graph.Add(x, [ y ])
        graph
    else
        match List.tryFind ((=) y) graph.[x] with
        | None ->
            graph.[x] <- y :: graph.[x]
            graph
        | Some _ -> graph

let private shortestPath<'a when 'a: equality> (state: State<'a>) (graph: T<'a>) (init: 'a) =
    let rec aux =
        function
        | [] -> ()
        | x :: xs ->
            if not <| Seq.contains x graph.Keys then
                aux xs
            else
                let dists = graph.[x]
                let current = Option.unwrap state.[x]

                let xs: 'a list =
                    List.fold
                        (fun (acc: 'a list) (dist: 'a) ->
                            if state.ContainsKey dist then
                                match state.[dist] with
                                | None ->
                                    state.[dist] <- Some(x :: current)
                                    dist :: acc
                                | Some path ->
                                    if not <| List.contains dist current
                                       && List.length current + 1 > List.length path then
                                        state.[dist] <- Some(x :: current)
                                        dist :: acc
                                    else
                                        acc
                            else
                                acc)
                        xs
                        dists

                aux xs

    aux [ init ]

let longestPath<'a when 'a: equality> (domainNodes: 'a list) (graph: T<'a>): Path<'a> =
    List.fold
        (fun (acc: Path<'a>) node ->
            let state = new State<'a>()
            List.iter (fun node -> state.Add(node, None)) domainNodes
            state.[node] <- Some []
            shortestPath state graph node

            List.iter
                (fun key ->
                    match state.[key] with
                    | None -> ()
                    | Some path -> state.[key] <- Some(key :: path))
                (List.ofSeq state.Keys)

            let longestPath =
                Seq.fold
                    (fun (acc: 'a list option) path ->
                        match acc, path with
                        | _, None -> acc
                        | None, _ -> path
                        | Some acc, Some path -> if List.length path < List.length acc then Some acc else Some path)
                    None
                    state.Values

            match longestPath with
            | None -> acc
            | Some path when List.length acc > List.length path -> acc
            | Some path -> path)
        []
        domainNodes
