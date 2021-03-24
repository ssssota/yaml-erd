module Schema

open Util


type Key =
    | Key of string

    override self.ToString() =
        match self with
        | Key s -> s


type Path =
    | Path of Key list
    override self.ToString() =
        match self with
        | Path path ->
            path
            |> List.map (fun key -> key.ToString())
            |> String.concat "."

type ScalarVal = ScalarVal of string

type EntityName =
    | EntityName of string
    override self.ToString() =
        match self with
        | EntityName name -> name

[<StructuralEquality;NoComparison>]
type Struct =
    | Scalar of ScalarVal * Position
    | Record of Map<Key, Struct> * Position

type RelationKind =
    | Unknown
    | One
    | ZeroOrOne
    | ZeroOrMore
    | OneOrMore

[<StructuralEquality;NoComparison>]
type Relation =
    { Src: Path list
      Dist: EntityName * Path list
      Kind: RelationKind * RelationKind
      Pos: Position }

[<StructuralEquality;NoComparison>]
type Entity =
    { Struct: Map<Key, Struct>
      Relations: Relation list
      Pos: Position }

module Path =
    let rec existsInStruct strct (Path path) =
        match strct with
        | Scalar _ -> List.isEmpty path
        | Record (fields, _) ->
            match path with
            | [] -> false
            | head :: tail -> Map.exists (fun key v -> key = head && existsInStruct v (Path tail)) fields

    let existsInEntity entity path =
        existsInStruct
        <| Record(entity.Struct, entity.Pos)
        <| path

type Group =
    { Entities: EntityName list
      Pos: Position }

type T =
    { Entities: Map<EntityName, Entity>
      Groups: Group list }
