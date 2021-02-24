module CalcOrder

open Util
open Schema

let private makeColumns (schema: Schema.T): EntityName list list =
    let graph =
        Map.fold
            (fun acc entityName entity ->
                List.fold (fun acc relation -> Graph.add (entityName, fst relation.Dist) acc) acc entity.Relations)
            Graph.empty<EntityName>
            schema.Entities

    let entityNames =
        List.filterMap
            (fun (entityName, _) ->
                if List.exists (fun (group: Group) -> List.contains entityName group.Entities) schema.Groups
                then None
                else Some entityName)
        <| Map.toList schema.Entities

    let groups =
        List.map (fun (groups: Group) -> groups.Entities) schema.Groups

    let rec aux acc entityNames =
        let longest = Graph.longestPath entityNames graph

        if List.isEmpty longest then
            acc
        else
            aux (longest :: acc)
            <| List.minus entityNames longest

    aux groups entityNames |> List.rev


let private makeRows (entities: Map<EntityName, Entity>) (rows: EntityName list array): EntityName list list =
    let graph: Graph.T<int> =
        Array.fold
            (fun (graph, srcIdx) (row: EntityName list) ->
                List.fold
                    (fun (graph: Graph.T<int>) (entityName: EntityName) ->
                        let entity = Map.find entityName entities

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


let calcOrder (schema: Schema.T): EntityName list list =
    makeColumns schema
    |> Array.ofList
    |> makeRows schema.Entities
