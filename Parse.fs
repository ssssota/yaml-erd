module Parse

open System
open System.IO
open System.Text
open System.Text.RegularExpressions
open YamlDotNet.RepresentationModel
open Util
open Schema

let private parseError (node: YamlNode) (msg: string): Result<'a> =
    Error [ { StartLine = node.Start.Line
              StartColumn = node.Start.Column
              EndLine = node.End.Line
              EndColumn = node.End.Column
              Message = msg } ]

let rec private parseStruct (node: YamlNode): Result<Struct> =
    match node with
    | :? YamlMappingNode as recordNode -> parseRecord recordNode |> Result.mapOk Record
    | :? YamlScalarNode as scalarNode -> parseScalar scalarNode |> Result.mapOk Scalar
    | _ ->
        parseError node
        <| String.Format("{0} must be mapping node or scalar node", node.ToString())

and private parseRecord (node: YamlNode): Result<(Key * Struct) list> =
    match node with
    | :? YamlMappingNode as recordYaml ->
        Seq.fold (fun acc key ->
            let key: YamlScalarNode = downcast box key
            let strct = parseStruct recordYaml.Children.[key]
            Result.merge (fun acc strct -> (key.ToString(), strct) :: acc) acc strct)
        <| Result.mkOk []
        <| Seq.rev recordYaml.Children.Keys
    | _ ->
        parseError node
        <| String.Format("{0} must be mapping node", node.ToString())

and private parseScalar (node: YamlNode): Result<string> =
    match node with
    | :? YamlScalarNode as scalarNode -> Result.mkOk scalarNode.Value
    | _ ->
        parseError node
        <| String.Format("{0} must be scalar node", node.ToString())

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
        <| String.Format("{0} is not valid relation head", node.ToString())

let private parseRelation (node: YamlNode): Result<Relation> =
    let node =
        match node with
        | :? YamlScalarNode as relationNode -> Result.mkOk relationNode
        | _ ->
            parseError node
            <| String.Format("{} must be a scalar node", node.ToString())

    node
    |> Result.bind (fun relationNode ->
        let regex =
            Regex
                ("^\\((?<src>.+?)(, (?<srcTail>.+?))*\\) (?<kindLeft>.*?)--(?<kindRight>.*?) (?<distEntity>.+?)\\((?<distField>.+?)\\)$")

        let m = regex.Match(relationNode.Value)
        if m.Success then
            Result.mkOk (m, relationNode)
        else
            parseError relationNode
            <| String.Format("`{0}` is not acceptable as relation", node.ToString()))
    |> Result.bind (fun (m, node) ->
        let srcHead = m.Groups.["src"].Value

        let srcTails =
            Seq.map (fun (capture: Capture) -> capture.Value) m.Groups.["srcTail"].Captures
            |> Seq.toList

        let kindLeft =
            parseRelationKind node true m.Groups.["kindLeft"].Value

        let kindRight =
            parseRelationKind node false m.Groups.["kindRight"].Value

        Result.merge (fun kindLeft kindRight ->
            { Src = srcHead :: srcTails
              Dist = m.Groups.["distEntity"].Value, m.Groups.["distField"].Value
              Kind = kindLeft, kindRight }) kindLeft kindRight)

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
            parseRecord node.[YamlScalarNode("struct")]

        Result.merge (fun relations strct ->
            { Name = name
              Struct = strct
              Relations = relations }) relations record

    | _ ->
        parseError node
        <| String.Format("{0} must be a mapping node", node.ToString())

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
