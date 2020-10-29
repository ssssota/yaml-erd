module Validation

open System
open Util
open Schema

let rec private hasStructField (strct: Struct) (field: string list): bool =
    match strct with
    | Scalar _ -> List.isEmpty field
    | Record fields_ ->  not (List.isEmpty field) && List.exists (fun (key, v) -> key = field.Head && hasStructField v field.Tail) fields_


let private hasEntityField (schema: Schema.T) (entityName: string) (field: string): bool =
    List.fold (fun acc (entity: Entity) ->
        acc
        || entityName = entity.Name
        && hasStructField
           <| Record entity.Struct
           <| Seq.toList (field.Split '.')) false schema

(* TODO *)

let private validateError (msg: string): ErrorValue =
    {
        StartLine = 0
        StartColumn = 0
        EndLine = 0
        EndColumn = 0
        Message = msg }

let validateRelation (schema: Schema.T): Util.Warning list * Util.ErrorValue list =
    List.fold (fun acc (entity: Entity) ->
        List.fold (fun (accWarnings, accErrors) (relation: Relation) ->
            let accErrors =
                List.fold
                    (fun acc src ->
                        if hasEntityField schema entity.Name src then
                            acc
                        else
                            (validateError <| String.Format("{0}.{1}", entity.Name, src)) :: acc
                    ) accErrors relation.Src
            let distEntity = List.find (fun entity -> entity.Name = fst relation.Dist) schema
            let loopRelations =
                List.filter (fun (distRelation: Relation) ->
                    fst distRelation.Dist = entity.Name
                    && List.contains (snd distRelation.Dist) relation.Src
                    && List.contains (snd relation.Dist) distRelation.Src)
                    distEntity.Relations
            let accErrors =
                List.tryHead loopRelations
                |> Option.map (fun relationRev ->
                    String.Format(
                        "loop relations found! {0}.{1} <-> {2}.{3}",
                        fst relationRev.Dist, snd relationRev.Dist,
                        fst relation.Dist, snd relation.Dist)
                    |> validateError
                )
                |> List.appendOpt accErrors
            let accErrors =
                if hasEntityField schema <| fst relation.Dist <| snd relation.Dist then accErrors
                else (validateError <| String.Format("{0}.{1}", fst relation.Dist, snd relation.Dist)) :: accErrors
            accWarnings, accErrors
        ) acc entity.Relations
    ) ([], []) schema

let validate (schema: Schema.T): Result<Schema.T> =
    let warnings, errors = validateRelation schema
    if List.isEmpty errors then Ok { Data = schema; Warnings = warnings}
    else Error errors
