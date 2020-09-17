module Schema

type Key = string

type Struct =
    | Scalar of string
    | Record of (Key * Struct) list

type RelationKind =
    | One
    | ZeroOrOne
    | ZeroOrMore
    | OneOrMore

type Relation =
    { Src: string list
      Dist: string * string
      Kind: RelationKind * RelationKind }

[<Struct>]
type Entity =
    { Name: string
      Struct: (Key * Struct) list
      Relations: Relation list }

type T = Entity list
