module PostProc

open Util
open Schema


let private makeBaseRelation (binRels: (string * string) list): string list * (string * string) list =
    let rec mkSubRelation start binRels =
        let found, binRels = List.findPop (fun (src, _) -> src = start) binRels
        match found with
        | Option.None -> [], binRels
        | Some (src, dst) ->
            let sub, binRels = mkSubRelation dst binRels
            src :: sub, binRels

    match binRels with
    | [] -> [], []
    | (src, dst) :: y ->
        let subRels, rest = mkSubRelation dst y
        src :: subRels, rest
        

let private calcOrder (schema: Schema.T): string list =
    let relations =
        List.fold (fun (acc: (string * string) list) (entity: Entity) ->
            List.fold (fun (acc: (string * string) list) (relation: Relation) ->
                (entity.Name, fst relation.Dist) :: acc
            ) acc entity.Relations
        ) [] schema
    let baseRelation, rest = makeBaseRelation relations
    baseRelation

let rec private sortByOrder (schema: Schema.T) (order: string list) : Schema.T =
    match order with
    | [] -> schema
    | hd :: tl ->
        let Some entity, schema = List.findPop (fun (entity: Entity) -> entity.Name = hd) schema
        let tl = sortByOrder schema tl
        entity :: tl

let postProc (schema: Schema.T) =
    let order = calcOrder schema
    sortByOrder schema order |> List.rev |> Result.mkOk

