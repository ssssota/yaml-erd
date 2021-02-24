module Validate

open System
open Util
open Schema

type UnknownEntityError = { Name: EntityName; Pos: Position }

type UnknownPathError =
    { EntityName: EntityName
      Path: Path
      Pos: Position }

type MutualRelationError =
    { EntityName1: EntityName
      Field1: Path
      RefPos1: Position
      EntityName2: EntityName
      Field2: Path
      RefPos2: Position }

type DuplicateEntityNameError =
    { EntityName: EntityName
      Group1: Group
      Group2: Group }

let private stringOfPos pos =
    $"{pos.StartLine}:{pos.StartColumn}-{pos.EndLine}:{pos.EndColumn}"

type ValidateError =
    | UnknownEntity of UnknownEntityError
    | UnknownPath of UnknownPathError
    | MutualRelations of MutualRelationError
    | DuplicateEntityName of DuplicateEntityNameError
    override self.ToString() =
        match self with
        | UnknownEntity err ->
            $"\x1b[31m validation error[{stringOfPos err.Pos}]: \x1b[0m there is no entity named `{err.Name}`"
        | UnknownPath err ->
            $"\x1b[31m validation error[{stringOfPos err.Pos}]: \x1b[0m entity `{err.EntityName}` does not have field `{
                                                                                                                            err.Path
            }`"
        | MutualRelations err ->
            $"\x1b[31m validation error[{stringOfPos err.RefPos1} and {stringOfPos err.RefPos2}]: \x1b[0m mutual relations are detected. `{
                                                                                                                                               err.EntityName1
            }:{err.Field1}` <-> `{err.EntityName2}:{err.Field2}`"
        | DuplicateEntityName err ->
            $"\x1b[31m validation error[{stringOfPos err.Group1.Pos} and {stringOfPos err.Group2.Pos}]: \x1b[0m duplicate entity name detected. {
                                                                                                                                                     err.EntityName
            } in {err.Group1.Entities} and {err.Group2.Entities}"

type ValidateResult<'a> = ExResult<'a, unit, ValidateError>

let private validateEntity (acc: ValidateError list) (entity: Entity): ValidateError list = acc

let private validateRelation (acc: ValidateError list)
                             (srcEntityName: EntityName)
                             (relation: Relation)
                             (schema: Schema.T)
                             : ValidateError list =
    let srcEntity = schema.Entities.TryFind srcEntityName

    let distEntity =
        schema.Entities.TryFind <| fst relation.Dist

    let checkUnknownPathErrors acc name entity paths =
        List.fold
            (fun acc path ->
                if Path.existsInEntity entity path then
                    acc
                else
                    UnknownPath
                        { EntityName = name
                          Path = path
                          Pos = relation.Pos }
                    :: acc)
            acc
            paths

    let checkMutualRelationsErrors (acc: ValidateError list) (distRelation: Relation) =
        if fst distRelation.Dist <> srcEntityName then
            acc
        else
            let forward =
                List.tryFind (fun dist -> List.contains dist distRelation.Src) (snd relation.Dist)

            let backward =
                List.tryFind (fun dist -> List.contains dist relation.Src) (snd distRelation.Dist)

            match forward, backward with
            | Some (forward), Some (backward) ->
                MutualRelations
                    { EntityName1 = srcEntityName
                      Field1 = backward
                      RefPos1 = distRelation.Pos
                      EntityName2 = fst relation.Dist
                      Field2 = forward
                      RefPos2 = relation.Pos }
                :: acc
            | _, _ -> acc

    match srcEntity, srcEntityName, relation.Src, distEntity, fst relation.Dist, snd relation.Dist with
    | Some (srcEntity), srcEntityName, srcPaths, Some (distEntity), distEntityName, distPaths ->
        let acc =
            checkUnknownPathErrors acc srcEntityName srcEntity srcPaths

        let acc =
            checkUnknownPathErrors acc distEntityName distEntity distPaths

        let acc =
            List.fold checkMutualRelationsErrors acc distEntity.Relations

        acc
    | Some (entity), entityName, paths, None, unknownEntityName, _
    | None, unknownEntityName, _, Some (entity), entityName, paths ->
        let acc =
            checkUnknownPathErrors acc entityName entity paths

        UnknownEntity
            { Name = unknownEntityName
              Pos = relation.Pos }
        :: acc
    | None, srcEntityName, _, None, distEntityName, _ ->
        UnknownEntity
            { Name = srcEntityName
              Pos = relation.Pos }
        :: UnknownEntity
            { Name = distEntityName
              Pos = relation.Pos }
           :: acc

let validate (schema: Schema.T): ValidateResult<Schema.T> =
    let errors: ValidateError list =
        Map.fold
            (fun (acc: ValidateError list) (entityName: EntityName) (entity: Entity) ->
                let acc = validateEntity acc entity
                List.fold (fun acc relation -> validateRelation acc entityName relation schema) acc entity.Relations)
            []
            schema.Entities

    let errors: ValidateError list =
        List.fold
            (fun (errors: ValidateError list, map: Map<EntityName, Group>) (focusedGroup: Group) ->
                List.fold
                    (fun (error, map) entityName ->
                        let map = Map.add entityName focusedGroup map

                        match Map.tryFind entityName map with
                        | Some existingGroup ->
                            let errors =
                                DuplicateEntityName
                                    { EntityName = entityName
                                      Group1 = existingGroup
                                      Group2 = focusedGroup }
                                :: errors

                            (errors, map)
                        | None -> (error, map))
                    (errors, map)
                    focusedGroup.Entities)
            (errors, Map.empty)
            schema.Groups
        |> fst

    let errors = errors |> List.rev |> List.toArray

    if Array.isEmpty errors then ExResult.mkOk schema else Error errors
