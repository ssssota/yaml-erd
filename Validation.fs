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

let private validateError (msg: string): Result<'a> =
    Error [ { StartLine = 0
              StartColumn = 0
              EndLine = 0
              EndColumn = 0
              Message = msg } ]

let private validateRelation (schema: Schema.T): Result<Schema.T> =
    List.fold (fun (schema: Result<Schema.T>) (entity: Entity) ->
        List.fold (fun (schema: Result<Schema.T>) relation ->
            let schema: Result<Schema.T> =
                List.fold (fun schema src ->
                    schema
                    |> Result.bind (fun schema ->
                        if hasEntityField schema entity.Name src then
                            Result.mkOk schema
                        else
                            validateError
                            <| String.Format("{0}.{1}", entity.Name, src)))

                <| schema
                <| relation.Src

            Result.bind (fun schema ->
                if hasEntityField schema
                   <| fst relation.Dist
                   <| snd relation.Dist then
                    Result.mkOk schema
                else
                    validateError
                    <| String.Format("{0}.{1}", fst relation.Dist, snd relation.Dist)) schema)
        <| schema
        <| entity.Relations)
    <| Result.mkOk schema
    <| schema


let validate (schema: Schema.T) =
    Result.mkOk schema |> Result.bind validateRelation
