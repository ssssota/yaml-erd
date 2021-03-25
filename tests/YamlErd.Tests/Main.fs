module YamlErd.Test

open Expecto
open Util
open Parse
open Schema

let makePos startLine startColumn endLine endColumn =
    { StartLine = startLine
      StartColumn = startColumn
      EndLine = endLine
      EndColumn = endColumn }

let parseTestCases =
    [ ("parse-error.yaml",
       "parsing error test",
       fun (result: ParseResult<Schema.T>) ->
           let expecteds =
               [ NeededKey "struct"
                 InvalidRelation
                 InvalidRelationKind "Invalid"
                 InvalidRelationKind "Type" ]

           Expect.wantError result "parsing should fail"
           |> List.ofArray
           |> List.zip expecteds
           |> List.iter (fun (expected, actual) -> Expect.equal actual.Kind expected "unexpected error"))
      ("empty.yaml",
       "parsing empty schema test",
       fun (result: ParseResult<Schema.T>) ->
           let errors =
               Expect.wantError result "parsing should fail"

           match errors with
           | [| error |] ->
               Expect.equal
                   error.Kind
                   (MustBe([ MappingType ], "schema"))
                   "error kind should be \"schema should be mapping-node\""
           | _ -> failwith "there should be only one error")
      ("single.yaml",
       "parsing simple schema test",
       fun (result: ParseResult<Schema.T>) ->
           let schema =
               Expect.wantOk result "parsing should success"

           Expect.isEmpty schema.Warnings "there should be no warnings"
           Expect.isEmpty schema.Data.Groups "non-grouped schema was expected"

           match schema.Data.Entities with
           | Map.Found (EntityName "Hoge") (entity, rest) ->
               Expect.isEmpty (Map.toSeq rest) "schema should have only one entity"
               Expect.isEmpty entity.Relations "schema should not have any relations"

               match entity.Struct with
               | Map.Founds [ Key "id"; Key "name" ]
                            ([ Scalar (ScalarVal "int", _); Scalar (ScalarVal "string", _) ], rest) ->
                   Expect.isEmpty rest "entity should not have any label except for `id` and `name`"
               | _ -> failwith "entity should contain `id: int` and `name: string`"
           | _ -> failwith "schema should have only one entity")
      ("without-groups.yaml",
       "parsing schema without groups test",
       fun (result: ParseResult<Schema.T>) ->
           let schema =
               Expect.wantOk result "parsing should success"

           Expect.isEmpty schema.Warnings "there should be no warnings"
           Expect.isEmpty schema.Data.Groups "non-grouped schema was expected"
           Expect.equal schema.Data.Entities.Count 12 "schema should contain 12 entities")
      ("with-groups.yaml",
       "parsing schema with groups test",
       fun (result: ParseResult<Schema.T>) ->
           let schema =
               Expect.wantOk result "parsing should success"

           Expect.isEmpty schema.Warnings "there should be no warnings"

           match schema.Data.Groups with
           | [ group1; group2 ] ->
               Expect.equal
                   group1.Entities
                   [ EntityName "Entity4"
                     EntityName "Entity5"
                     EntityName "Entity6" ]
                   "schema contained unexpected group"

               Expect.equal
                   group2.Entities
                   [ EntityName "Entity1"
                     EntityName "Entity12"
                     EntityName "Entity11" ]
                   "schema contained unexpected group"
           | _ -> failwith "schema should have just 2 groups") ]

[<Tests>]
let parseTests =
    testList "Parse Tests"
    <| List.map
        (fun (filename, caseName, check) ->
            testCase caseName
            <| fun () ->
                let actual =
                    Parse.schemaFromFile $"../../../testcases/{filename}"

                check actual)
        parseTestCases

open Validate

let validationShouldSuccess (result: ValidateResult<Schema.T>) =
    Expect.isOk result "validation should success"

let validationTestCases =
    [ ("validate-entities-error.yaml",
       "validation error test for entities",
       fun (result: ValidateResult<Schema.T>) ->
           let checks =
               [ function
                 | UnknownPath err ->
                     err.EntityName = EntityName "Entity0"
                     && err.Path = Path [ Key "InvalidField" ]
                 | _ -> false
                 function
                 | UnknownPath err ->
                     err.EntityName = EntityName "Entity1"
                     && err.Path = Path [ Key "InvalidField" ]
                 | _ -> false
                 function
                 | UnknownEntity err -> err.Name = EntityName "InvalidEntity"
                 | _ -> false
                 function
                 | MutualRelations err ->
                     err.EntityName1 = EntityName "Entity0"
                     && err.Field1 = Path [ Key "id" ]
                     && err.EntityName2 = EntityName "Entity1"
                     && err.Field2 = Path [ Key "id" ]
                 | _ -> false
                 function
                 | MutualRelations err ->
                     err.EntityName1 = EntityName "Entity1"
                     && err.Field1 = Path [ Key "id" ]
                     && err.EntityName2 = EntityName "Entity0"
                     && err.Field2 = Path [ Key "id" ]
                 | _ -> false ]

           let actualErrors =
               Expect.wantError result "validation should fail"

           Expect.equal actualErrors.Length checks.Length $"there should be just ${checks.Length} errors"

           actualErrors
           |> Array.iter
               (fun actualError ->
                   if List.exists (fun check -> check actualError) checks
                   then ()
                   else failwith $"unexpected error occurred!: {actualError.ToString()}"))
      ("validate-groups-error.yaml",
       "validation error test for groups",
       fun (result: ValidateResult<Schema.T>) ->
           let errors =
               Expect.wantError result "validation should fail"

           match List.ofArray errors with
           | [ DuplicateEntityNameInGroups err ] ->
               Expect.equal err.EntityName (EntityName "Entity1") "duplicate entity name in groups should be `Entity1`"
           | errors -> failwith $"expected only one duplicate-entiy-name-in-group error, but got ${errors}")
      ("single.yaml",
       "validation simple schema test",
       fun (result: ValidateResult<Schema.T>) -> Expect.isOk result "validation should success")
      ("without-groups.yaml", "validation schema without groups test", validationShouldSuccess)
      ("with-groups.yaml", "validation schema with groups test", validationShouldSuccess)
      ("sample.yaml", "validation sample schema test", validationShouldSuccess) ]

[<Tests>]
let validationTests =
    testList "Validation Tests"
    <| List.map
        (fun (filename, caseName, check) ->
            testCase caseName
            <| fun () ->
                let actual =
                    Parse.schemaFromFile $"../../../testcases/{filename}"

                let actualSchema =
                    Expect.wantOk actual "parsing should success"

                Expect.isEmpty actualSchema.Warnings "there should be no warnings"
                let actual = Validate.validate actualSchema.Data
                check actual)
        validationTestCases

[<EntryPoint>]
let main argv =
    Tests.runTestsInAssembly defaultConfig argv
