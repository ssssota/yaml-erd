module Schema
open Util

type Key = string

type Struct =
    | Scalar of string * Position
    | Record of (Key * Struct) list * Position

type RelationKind =
    | Unknown
    | One
    | ZeroOrOne
    | ZeroOrMore
    | OneOrMore

type Relation =
    { Src: string list
      Dist: string * string list
      Kind: RelationKind * RelationKind
      Pos: Position }

type Entity =
    { Name: string
      Struct: (Key * Struct) list
      Relations: Relation list
      Pos: Position }

type T = Entity list
