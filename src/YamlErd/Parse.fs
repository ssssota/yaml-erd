module Parse

open System.IO
open System.Text
open System.Text.RegularExpressions
open YamlDotNet.RepresentationModel
open Util
open Schema

[<StructuralEquality;NoComparison>]
type NodeType =
    | ScalarType
    | SequenceType
    | MappingType
    override self.ToString() =
        match self with
        | ScalarType -> "scalar"
        | SequenceType -> "sequence"
        | MappingType -> "mapping"

[<StructuralEquality;NoComparison>]
type ErrorKind =
    | InvalidRelationKind of string
    | InvalidRelation
    | MustBe of NodeType list * string
    | NeededKey of string
    override self.ToString() =
        match self with
        | InvalidRelationKind s -> $"`{s}` is not valid relation head"
        | InvalidRelation -> "not valid as relation"
        | MustBe (types, str) ->
            let opt =
                types
                |> List.map (fun typ -> typ.ToString())
                |> String.concat " or "

            $"{str} node must be {opt}"
        | NeededKey s -> $"this node must have key `{s}`"

[<StructuralEquality;NoComparison>]
type ParseError =
    { Pos: Position
      Kind: ErrorKind }
    override self.ToString() =
        let kind = self.Kind.ToString()
        $"\x1b[31m parse error[{self.Pos.StartLine}:{self.Pos.StartColumn}-{self.Pos.EndLine}:{self.Pos.EndColumn}]: \x1b[0m {kind}"

let private positionOfNode (node: YamlNode): Position =
    { StartLine = node.Start.Line
      StartColumn = node.Start.Column
      EndLine = node.End.Line
      EndColumn = node.End.Column }

type ParseResult<'a> = ExResult<'a, unit, ParseError>

let private parseError<'a> k (node: YamlNode): ParseResult<'a> =
    ExResult.mkError { Pos = positionOfNode node; Kind = k }

let rec private parseStruct (node: YamlNode): ParseResult<Struct> =
    match node with
    | :? YamlMappingNode as recordNode ->
        parseRecord recordNode
        |> ExResult.map (fun x -> Record(x, positionOfNode node))
    | :? YamlScalarNode as scalarNode ->
        parseStructLeaf scalarNode
        |> ExResult.map (fun x -> Scalar(x, positionOfNode node))
    | _ -> parseError (MustBe([ MappingType; ScalarType ], "struct")) node

and private parseRecord (node: YamlMappingNode) =
    Seq.fold
        (fun acc key ->
            let key: YamlScalarNode = downcast box key
            let strct = parseStruct node.Children.[key]
            ExResult.merge (fun map strct -> Map.add (Key key.Value) strct map) acc strct)
    <| ExResult.mkOk Map.empty
    <| Seq.rev node.Children.Keys

and private parseStructLeaf (node: YamlScalarNode) = ExResult.mkOk <| ScalarVal node.Value

let private parseRelationKind (node: YamlNode) (isLeft: bool) (str: string) =
    match isLeft, str with
    | _, "" -> ExResult.mkOk Unknown
    | _, "||" -> ExResult.mkOk One
    | true, "|o"
    | false, "o|" -> ExResult.mkOk ZeroOrOne
    | true, "}o"
    | false, "o{" -> ExResult.mkOk ZeroOrMore
    | true, "}|"
    | false, "|{" -> ExResult.mkOk OneOrMore
    | _, _ -> parseError (InvalidRelationKind str) node

let private parseRelation (node: YamlNode): ParseResult<Relation> =
    match node with
    | :? YamlScalarNode as relationNode ->
        let relationRegex =
            Regex(
                "^\\((?<src>.+?)(, (?<srcTail>.+?))*\\) (?<kindLeft>.*?)--(?<kindRight>.*?) (?<distEntity>.+?)\\((?<distPathHead>.+?)(, (?<distPathTail>.+?))*\\)$"
            )

        let m = relationRegex.Match(relationNode.Value)

        if m.Success then
            let srcHead =
                m.Groups.["src"].Value.Split "."
                |> Array.map Key
                |> Array.toList
                |> Path

            let srcTails =
                Seq.map
                    (fun (capture: Capture) ->
                        capture.Value.Split "."
                        |> Array.map Key
                        |> Array.toList
                        |> Path)
                    m.Groups.["srcTail"].Captures
                |> Seq.toList

            let src = srcHead :: srcTails
            let distEntity = EntityName m.Groups.["distEntity"].Value

            let distPathHead =
                m.Groups.["distPathHead"].Value.Split "."
                |> Array.map Key
                |> Array.toList
                |> Path

            let distPathTail =
                Seq.map
                    (fun (capture: Capture) ->
                        capture.Value.Split "."
                        |> Array.map Key
                        |> Array.toList
                        |> Path)
                    m.Groups.["distPathTail"].Captures
                |> Seq.toList

            let distPath = distPathHead :: distPathTail

            let kindLeft =
                parseRelationKind node true m.Groups.["kindLeft"].Value

            let kindRight =
                parseRelationKind node false m.Groups.["kindRight"].Value

            ExResult.merge
                (fun kindLeft kindRight ->
                    { Src = src
                      Dist = distEntity, distPath
                      Kind = kindLeft, kindRight
                      Pos = positionOfNode node })
                kindLeft
                kindRight
        else
            parseError InvalidRelation relationNode
    | _ -> parseError InvalidRelation node


let private parseEntity (node: YamlNode): ParseResult<Entity> =
    match node with
    | :? YamlMappingNode as node ->
        let record =
            if node.Children.ContainsKey(YamlScalarNode("struct")) then
                match node.[YamlScalarNode("struct")] with
                | :? YamlMappingNode as structNode -> parseRecord structNode
                | _ -> parseError (MustBe([ MappingType ], "struct")) node
            else
                parseError (NeededKey "struct") node

        let relations =
            if node.Children.ContainsKey(YamlScalarNode("relations")) then
                match node.[YamlScalarNode("relations")] with
                | :? YamlSequenceNode as relationsNode ->
                    Seq.fold
                        (fun acc x ->
                            let relation = parseRelation x
                            ExResult.merge (fun acc relation -> relation :: acc) acc relation)
                    <| ExResult.mkOk []
                    <| relationsNode.Children
                | :? YamlScalarNode -> ExResult.mkOk []
                | node -> parseError (MustBe([ SequenceType; ScalarType ], "relations")) node
            else
                ExResult.mkOk []

        ExResult.merge
            (fun record relations ->
                { Struct = record
                  Relations = relations
                  Pos = positionOfNode node })
            record
            relations

    | _ -> parseError (MustBe([ MappingType ], "entity")) node

let private parseSchema (node: YamlNode) =
    match node with
    | :? YamlMappingNode as schema ->
        Seq.fold
            (fun acc key ->
                let key: YamlScalarNode = downcast box key
                let entityName = EntityName key.Value
                let entity = parseEntity schema.[key]
                ExResult.merge (fun table entity -> Map.add entityName entity table) acc entity)
        <| ExResult.mkOk Map.empty
        <| schema.Children.Keys
    | _ -> parseError (MustBe([ MappingType ], "schema")) node

(*
    e.g. [Entity4, Entity5, Entity6]
*)
let private parseGroup (node: YamlNode) =
    match node with
    | :? YamlSequenceNode as group ->
        Seq.fold
            (fun acc (entityNameNode: YamlNode) ->
                match entityNameNode with
                | :? YamlScalarNode as entityName -> ExResult.map (fun acc -> EntityName entityName.Value :: acc) acc
                | _ ->
                    ExResult.merge (fun acc _ -> acc) acc
                    <| parseError (MustBe([ ScalarType ], "element in group")) entityNameNode)
            (ExResult.mkOk [])
            group.Children
        |> ExResult.map
            (fun entityNames ->
                { Entities = List.rev entityNames
                  Pos = positionOfNode node })
    | _ -> parseError (MustBe([ SequenceType ], "group")) node

(*
    e.g.
    - [Entity1, Entity2, Entity3]
    - [Entity4, Entity5, Entity6]
*)
let private parseGroups (node: YamlNode): ParseResult<Schema.Group list> =
    match node with
    | :? YamlSequenceNode as groups ->
        Seq.fold
            (fun acc (group: YamlNode) -> ExResult.merge (fun acc group -> group :: acc) acc (parseGroup group))
            (ExResult.mkOk [])
            groups.Children
        |> ExResult.map List.rev
    | _ -> parseError (NeededKey "group") node

let schemaFromFile (filename: string): ParseResult<T> =
    let yaml = YamlStream()
    yaml.Load(new StreamReader(filename, Encoding.UTF8))

    match yaml.Documents.[0].RootNode with
    | :? YamlMappingNode as mapping ->
        if mapping.Children.ContainsKey(YamlScalarNode("schema")) then
            let entities =
                parseSchema mapping.[YamlScalarNode("schema")]

            let groups =
                if mapping.Children.ContainsKey(YamlScalarNode("group"))
                then parseGroups mapping.[YamlScalarNode("group")]
                else ExResult.mkOk []

            ExResult.merge (fun entities groups -> { Entities = entities; Groups = groups }) entities groups
        else
            parseError (NeededKey "schema") mapping
    | node -> parseError (MustBe([ MappingType ], "toplevel")) node
