module Validation

open System
open Util
open Schema

type ValidationWarning = Dummy

type UnknownEntityError = {
    Name: string
    Pos: Position
}
type UnknownFieldError = {
    EntityName: string
    FieldName: string
    Pos: Position
}
type MutualRelationError = {
    EntityName1: string
    FieldName1: string
    RefPos1: Position
    EntityName2: string
    FieldName2: string
    RefPos2: Position
}
type ValidationError =
    | UnknownEntity of UnknownEntityError
    | UnknownField of UnknownFieldError
    | MutualRelations of MutualRelationError
    override self.ToString() =
        match self with
        | UnknownEntity err ->
            String.Format("\x1b[31m validation error[{0}:{1}-{2}:{3}]: \x1b[0m there is no entity named `{4}`", err.Pos.StartLine, err.Pos.StartColumn, err.Pos.EndLine, err.Pos.EndColumn, err.Name)
        | UnknownField err ->
            String.Format("\x1b[31m validation error[{0}:{1}-{2}:{3}]: \x1b[0m entity `{4}` does not have field `{5}`", err.Pos.StartLine, err.Pos.StartColumn, err.Pos.EndLine, err.Pos.EndColumn, err.EntityName, err.FieldName)
        | MutualRelations err ->
            String.Format(
                "\x1b[31m validation error[{0}:{1}-{2}:{3} and {4}:{5}-{6}:{7}]: \x1b[0m mutual relations are detected. `{8}:{9}` <-> `{10}:{11}`",
                err.RefPos1.StartLine, err.RefPos1.StartColumn, err.RefPos1.EndLine, err.RefPos1.EndColumn,
                err.RefPos1.StartLine, err.RefPos1.StartColumn, err.RefPos1.EndLine, err.RefPos1.EndColumn,
                err.EntityName1, err.FieldName1, err.EntityName2, err.FieldName2)

let rec private hasStructField (strct: Struct) (field: string list): bool =
    match strct with
    | Scalar _ -> List.isEmpty field
    | Record (fields_, _) ->  not (List.isEmpty field) && List.exists (fun (key, v) -> key = field.Head && hasStructField v field.Tail) fields_

let private hasEntityField (schema: Schema.T) (entityName: string) (field: string): bool =
    List.exists (fun (entity: Entity) ->
        let field = Seq.toList (field.Split '.')
        entityName = entity.Name && hasStructField <| Record (entity.Struct, entity.Pos) <| field
    ) schema

let validateRelation (schema: Schema.T): obj list * obj list =
    List.fold (fun acc (entity: Entity) ->
        List.fold (fun (accWarnings, accErrors) (relation: Relation) ->
            let accErrors =
                List.fold
                    (fun acc src ->
                        if hasEntityField schema entity.Name src then
                            acc
                        else
                            UnknownField {EntityName = entity.Name; FieldName = src; Pos = relation.Pos} :> obj:: acc
                    ) accErrors relation.Src
            let accErrors =
                match List.tryFind (fun (entity: Entity) -> entity.Name = fst relation.Dist) schema with
                | None ->
                    let err = UnknownEntity {
                        Name = fst relation.Dist
                        Pos = relation.Pos
                    }
                    err :> obj :: accErrors
                | Some distEntity ->
                    let loopRelations =
                        List.filter (fun (distRelation: Relation) ->
                            fst distRelation.Dist = entity.Name
                            && List.contains (snd distRelation.Dist) relation.Src
                            && List.contains (snd relation.Dist) distRelation.Src)
                            distEntity.Relations
                    List.tryHead loopRelations
                        |> Option.map (fun relationRev ->
                            MutualRelations {
                                EntityName1 = fst relationRev.Dist
                                FieldName1 = snd relationRev.Dist
                                RefPos1 = relationRev.Pos
                                EntityName2 = fst relation.Dist
                                FieldName2 = fst relation.Dist
                                RefPos2 = relation.Pos
                            } :> obj
                    )
                    |> List.appendOpt accErrors
            let accErrors =
                if hasEntityField schema <| fst relation.Dist <| snd relation.Dist then accErrors
                else UnknownField { EntityName = fst relation.Dist; FieldName = snd relation.Dist; Pos = relation.Pos} :> obj :: accErrors
            accWarnings, accErrors
        ) acc entity.Relations
    ) ([], []) schema

let validate (schema: Schema.T): Result<Schema.T> =
    let warnings, errors = validateRelation schema
    if List.isEmpty errors then Ok { Data = schema; Warnings = warnings}
    else Error errors
