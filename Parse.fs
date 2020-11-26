module Parse

open System
open System.IO
open System.Text
open System.Text.RegularExpressions
open YamlDotNet.RepresentationModel
open Util
open Schema

type ParseWarning =
    {
        Pos: Position
        Message: string
    }
    override self.ToString() =
        String.Format("\x1b[33m parse warning[{0}:{1}-{2}:{3}]: \x1b[0m {4}", self.Pos.StartLine, self.Pos.StartColumn, self.Pos.EndLine, self.Pos.EndColumn, self.Message)

type ParseError =
    {
        Pos: Position
        Message: string
    }
    override self.ToString() =
        String.Format("\x1b[31m parse error[{0}:{1}-{2}:{3}]: \x1b[0m {4}", self.Pos.StartLine, self.Pos.StartColumn, self.Pos.EndLine, self.Pos.EndColumn, self.Message)

let private positionOfNode (node: YamlNode): Position =
    {
        StartLine = node.Start.Line
        StartColumn = node.Start.Column
        EndLine = node.End.Line
        EndColumn = node.End.Column
    }

let private parseError (node: YamlNode) (msg: string): Result<'a> =
    Error [
        {
            Pos = positionOfNode node
            Message = msg
        }
    ]

let rec private parseStruct (node: YamlNode): Result<Struct> =
    match node with
    | :? YamlMappingNode as recordNode -> parseRecord recordNode |> Result.mapOk (fun x -> Record (x, positionOfNode node))
    | :? YamlScalarNode as scalarNode -> parseStructLeaf scalarNode |> Result.mapOk (fun x -> Scalar (x, positionOfNode node))
    | _ -> parseError node "struct node must be either mapping or scalar"

and private parseRecord (node: YamlMappingNode): Result<(Key * Struct) list> =
    Seq.fold (fun acc key ->
        let key: YamlScalarNode = downcast box key
        let strct = parseStruct node.Children.[key]
        Result.merge (fun acc strct -> (key.ToString(), strct) :: acc) acc strct)
    <| Result.mkOk []
    <| Seq.rev node.Children.Keys

and private parseStructLeaf (node: YamlScalarNode): Result<string> = Result.mkOk node.Value

let private parseRelationKind (node: YamlNode) (isLeft: bool) (str: string): Result<RelationKind> =
    match isLeft, str with
    | _, "" -> Result.mkOk Unknown
    | _, "||" -> Result.mkOk One
    | true, "|o"
    | false, "o|" -> Result.mkOk ZeroOrOne
    | true, "}o"
    | false, "o{" -> Result.mkOk ZeroOrMore
    | true, "}|"
    | false, "|{" -> Result.mkOk OneOrMore
    | _, _ ->
        parseError node
        <| String.Format("`{0}` is not valid relation head", str)

let private parseRelation (node: YamlNode): Result<Relation> =
    let node =
        match node with
        | :? YamlScalarNode as relationNode -> Result.mkOk relationNode
        | _ -> parseError node "relation node must be scalar"

    node
    |> Result.bind (fun relationNode ->
        let regex =
            Regex
                ("^\\((?<src>.+?)(, (?<srcTail>.+?))*\\) (?<kindLeft>.*?)--(?<kindRight>.*?) (?<distEntity>.+?)\\((?<distField>.+?)(, (?<distFieldTail>.+?))*\\)$")

        let m = regex.Match(relationNode.Value)
        if m.Success then
            Result.mkOk (m, relationNode)
        else
            parseError relationNode
            <| String.Format("`{0}` is not acceptable as relation", relationNode.Value))
    |> Result.bind (fun (m, node) ->
        let srcHead = m.Groups.["src"].Value

        let srcTails =
            Seq.map (fun (capture: Capture) -> capture.Value) m.Groups.["srcTail"].Captures
            |> Seq.toList
        let src = srcHead :: srcTails

        let distEntity = m.Groups.["distEntity"].Value
        let distFieldHead = m.Groups.["distField"].Value
        let distFieldTails =
            Seq.map (fun (capture: Capture) -> capture.Value) m.Groups.["distFieldTail"].Captures
            |> Seq.toList
        let dist = distEntity, distFieldHead :: distFieldTails

        let kindLeft =
            parseRelationKind node true m.Groups.["kindLeft"].Value

        let kindRight =
            parseRelationKind node false m.Groups.["kindRight"].Value

        Result.merge (fun kindLeft kindRight ->
            { Src = src
              Dist = dist
              Kind = kindLeft, kindRight
              Pos = positionOfNode node}) kindLeft kindRight)

let private parseEntity (name: string) (node: YamlNode): Result<Entity> =
    match node with
    | :? YamlMappingNode as node ->

        let relations =
            if node.Children.ContainsKey(YamlScalarNode("relations")) then
                let relationsNode: YamlSequenceNode =
                    downcast node.[YamlScalarNode("relations")]

                Seq.fold (fun acc x ->
                    let relation = parseRelation x
                    Result.merge (fun acc relation -> relation :: acc) acc relation)
                <| Result.mkOk []
                <| relationsNode.Children
            else
                Result.mkOk []

        let record =
            if node.Children.ContainsKey(YamlScalarNode("struct")) then
                match node.[YamlScalarNode("struct")] with
                | :? YamlMappingNode as structNode -> parseRecord structNode
                | _ -> parseError node "struct node must be mapping"
            else
                parseError node <| String.Format("entity `{0}` must have `struct` data", name)

        Result.merge (fun relations strct ->
            { Name = name
              Struct = strct
              Relations = relations
              Pos = positionOfNode node}) relations record

    | _ ->
        parseError node "entity node must be a mapping node"

let private parseSchema (node: YamlNode) =
    match node with
    | :? YamlMappingNode as mapping ->
        match mapping.[YamlScalarNode("schema")] with
        | :? YamlMappingNode as schema ->
            Seq.fold (fun acc key ->
                let key: YamlScalarNode = downcast box key

                let entity = parseEntity key.Value schema.[key]
                Result.merge (fun acc entity -> entity :: acc) acc entity)
            <| Result.mkOk []
            <| schema.Children.Keys
        | _ -> parseError mapping "toplevel mapping must have `schema` field"
    | _ -> parseError node "toplevel must be a mapping node"

let schemaFromFile (filename: string): Result<T> =
    let yaml = YamlStream()
    yaml.Load(new StreamReader(filename, Encoding.UTF8))
    parseSchema (yaml.Documents.[0].RootNode) |> Result.mapOk List.rev
