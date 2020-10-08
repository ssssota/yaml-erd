module PostProc

open Util
open Schema

let private calcOrder (schema: Schema.T): string list =
    let relations =
        List.fold (fun (acc: (string * string) list) (entity: Entity) ->
            List.fold (fun (acc: (string * string) list) (relation: Relation) ->
                let newRel = entity.Name, fst relation.Dist
                if List.mem newRel acc then acc else newRel :: acc) acc entity.Relations) [] schema

    match relations with
    | [] -> []
    | (src, dst) :: relations ->
        let rec makeOrder first last relations order =
            let preFound, relations =
                List.findPop (fun (_, dst) -> dst = first) relations

            let postFound, relation =
                List.findPop (fun (src, _) -> src = last) relations

            match preFound, postFound with
            | Option.None, Option.None -> order
            | Some (src, _), Option.None -> makeOrder src dst relations <| src :: order
            | Option.None, Some (_, dst) -> makeOrder src dst relations <| order @ [ dst ] // TODO
            | Some (src, _), Some (_, dst) ->
                makeOrder src dst relations
                <| src
                :: order
                @ [ dst ]

        makeOrder src dst relations [ src; dst ]



let rec private sortByOrder (schema: Schema.T) (order: string list): (Entity list * Entity list) =
    match order with
    | [] -> [], schema
    | hd :: tl ->
        let Some entity, schema =
            List.findPop (fun (entity: Entity) -> entity.Name = hd) schema

        let tl, schema = sortByOrder schema tl
        entity :: tl, schema

let postProc (schema: Schema.T) =
    let order = calcOrder schema
    let fixedEntities, freeEntities = sortByOrder schema order
    Result.mkOk (List.rev fixedEntities, freeEntities)
