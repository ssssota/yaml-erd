module CalcOrder

open Util
open Schema

let private makeColumns (schema: Schema.T) (groups: string list list): string list list =
    let graph =
        List.fold
            (fun acc entity ->
                List.fold (fun acc relation -> Graph.add (entity.Name, fst relation.Dist) acc) acc entity.Relations)
            Graph.empty<string>
            schema

    let entityNames =
        List.filterMap
            (fun entity ->
                if List.exists (fun group -> List.contains entity.Name group) groups
                then None
                else Some entity.Name)
            schema

    let rec aux acc entityNames =
        let longest = Graph.longestPath entityNames graph

        if List.isEmpty longest then
            acc
        else
            aux (longest :: acc)
            <| List.minus entityNames longest

    aux groups entityNames |> List.rev


let private makeRows (schema: Schema.T) (rows: string list array): string list list =
    let graph: Graph.T<int> =
        Array.fold
            (fun (graph, srcIdx) (row: string list) ->
                List.fold
                    (fun (graph: Graph.T<int>) (entityName: string) ->
                        let entity =
                            List.find (fun entity -> entity.Name = entityName) schema

                        List.fold
                            (fun graph relation ->
                                let distIdx =
                                    Array.findIndex (List.contains (fst relation.Dist)) rows

                                if srcIdx = distIdx then graph else Graph.add (srcIdx, distIdx) graph)
                            graph
                            entity.Relations)
                    graph
                    row,
                (srcIdx + 1))
            (Graph.empty<int>, 0)
            rows
        |> fst

    let rec aux (acc: int list list) (entityIndices: int list) =
        let longest: int list = Graph.longestPath entityIndices graph

        if List.isEmpty longest then
            acc
        else
            aux (List.rev longest :: acc)
            <| List.minus entityIndices longest

    let indices =
        aux []
        <| List.ofSeq (seq { 0 .. (Array.length rows) - 1 })

    List.fold (fun acc indices -> List.fold (fun acc idx -> rows.[idx] :: acc) acc indices) [] indices
    |> List.rev


let calcOrder (schema: Schema.T) (groups: string list list): string list list =
    makeColumns schema groups
    |> Array.ofList
    |> makeRows schema
