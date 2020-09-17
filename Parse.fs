module Parse

open System
open System.IO
open System.Text
open System.Text.RegularExpressions
open YamlDotNet.RepresentationModel
open Util
open Schema

let parseError (node: YamlNode) (msg: string): Result<'a> =
    Error [ { StartLine = node.Start.Line
              StartColumn = node.Start.Column
              EndLine = node.End.Line
              EndColumn = node.End.Column
              Message = msg } ]

let rec structFromYamlNode (node: YamlNode): Result<Struct> =
    match node with
    | :? YamlMappingNode as recordYaml ->
        recordFromYamlNode recordYaml
        |> Result.map (fun { Data = data; Warnings = warnings } ->
            { Data = Record data
              Warnings = warnings })
    | :? YamlScalarNode as scalarYaml ->
        scalarFromYamlNode scalarYaml
        |> Result.map (fun { Data = data; Warnings = warnings } ->
            { Data = Scalar data
              Warnings = warnings })
    | _ -> parseError node (String.Format("{0} should be mapping node or scalar node", node.ToString()))

and recordFromYamlNode (node: YamlNode): Result<(Key * Struct) list> =
    match node with
    | :? YamlMappingNode as recordYaml ->
        Seq.fold (fun acc key ->
            let key: YamlScalarNode = downcast box key
            match acc, structFromYamlNode recordYaml.Children.[key] with
            | Ok { Data = data1; Warnings = warnings1 }, Ok { Data = data2; Warnings = warnings2 } ->
                Ok
                    { Data = (key.ToString(), data2) :: data1
                      Warnings = warnings1 @ warnings2 }
            | Error err1, Error err2 -> Error(err1 @ err2)
            | _, Error err
            | Error err, _ -> Error err) (Ok { Data = []; Warnings = [] }) (Seq.rev recordYaml.Children.Keys)
    | _ -> parseError node (String.Format("{0} should be mapping node", node.ToString()))

and scalarFromYamlNode (node: YamlNode): Result<string> =
    match node with
    | :? YamlScalarNode as scalarYaml ->
        Ok
            { Data = scalarYaml.Value
              Warnings = [] }
    | _ -> parseError node (String.Format("{0} should be scalar node", node.ToString()))

let relationKindFromYamlNode (node: YamlNode) (isLeft: bool) (str: string): Result<RelationKind> =
    match isLeft, str with
    | _, "||" ->
        Ok
            { Data = RelationKind.One
              Warnings = [] }
    | true, "|o"
    | false, "o|" ->
        Ok
            { Data = RelationKind.ZeroOrOne
              Warnings = [] }
    | true, "}o"
    | false, "o{" ->
        Ok
            { Data = RelationKind.ZeroOrMore
              Warnings = [] }
    | true, "}|"
    | false, "|{" ->
        Ok
            { Data = RelationKind.OneOrMore
              Warnings = [] }
    | _, _ -> parseError node (String.Format(""))

let relationFromYamlNode (node: YamlNode): Result<Relation> =
    let relationYaml: YamlScalarNode = downcast node

    let regex =
        Regex
            ("^\\((?<src>.+?)(, (?<srcTail>.+?))*\\) (?<kindLeft>.+?)--(?<kindRight>.+?) (?<distEntity>.+?)\\((?<distField>.+?)\\)$")

    let m = regex.Match(relationYaml.Value)
    if m.Success then
        let srcHead = m.Groups.["src"].Value

        let srcTails =
            Seq.map (fun (capture: Capture) -> capture.Value) m.Groups.["srcTail"].Captures
            |> Seq.toList

        let kindLeft =
            relationKindFromYamlNode node true m.Groups.["kindLeft"].Value

        let kindRight =
            relationKindFromYamlNode node false m.Groups.["kindRight"].Value

        match kindLeft, kindRight with
        | Ok { Data = kindLeft; Warnings = warnings1 }, Ok { Data = kindRight; Warnings = warnings2 } ->
            Ok
                { Data =
                      { Src = srcHead :: srcTails
                        Dist = m.Groups.["distEntity"].Value, m.Groups.["distField"].Value
                        Kind = kindLeft, kindRight }
                  Warnings = [] }
        | Error err1, Error err2 -> Error(err1 @ err2)
        | _, Error err
        | Error err, _ -> Error err
    else
        parseError node (String.Format("`{0}` is not acceptable as relation", node.ToString()))

let entityFromYamlNode (name: string) (node: YamlNode): Result<Entity> =
    let node: YamlMappingNode = downcast node

    let relations =
        if node.Children.ContainsKey(YamlScalarNode("relations")) then
            let relationsYaml: YamlSequenceNode =
                downcast node.[YamlScalarNode("relations")]

            Seq.fold (fun acc x ->
                let relation = relationFromYamlNode x
                match acc, relation with
                | Ok { Data = rs; Warnings = warnings1 }, Ok { Data = r; Warnings = warnings2 } ->
                    Ok
                        { Data = r :: rs
                          Warnings = warnings1 @ warnings2 }
                | Error err1, Error err2 -> Error(err1 @ err2)
                | _, Error err
                | Error err, _ -> Error err) (Ok { Data = []; Warnings = [] }) relationsYaml.Children
        else
            Ok { Data = []; Warnings = [] }

    let record =
        recordFromYamlNode node.[YamlScalarNode("struct")]

    match relations, record with
    | Ok { Data = rs; Warnings = warnings1 }, Ok { Data = strct; Warnings = warnings2 } ->
        Ok
            { Data =
                  { Name = name
                    Struct = strct
                    Relations = rs }
              Warnings = warnings1 @ warnings2 }
    | Error err1, Error err2 -> Error(err1 @ err2)
    | _, Error err
    | Error err, _ -> Error err

let schemaFromYamlNode (node: YamlNode): Result<Schema.T> =
    let mapping: YamlMappingNode = downcast node

    let schemaYaml: YamlMappingNode =
        downcast mapping.[YamlScalarNode("schema")]

    Seq.fold (fun acc key ->
        let key: YamlScalarNode = downcast box key

        let entity =
            entityFromYamlNode key.Value schemaYaml.[key]

        match acc, entity with
        | Ok { Data = es; Warnings = warnings1 }, Ok { Data = e; Warnings = warnings2 } ->
            Ok
                { Data = e :: es
                  Warnings = warnings1 @ warnings2 }
        | Error err1, Error err2 -> Error(err1 @ err2)
        | Error err, _
        | _, Error err -> Error err) (Ok { Data = []; Warnings = [] }) schemaYaml.Children.Keys

let schemaFromFile (filename: string) =
    let yaml = YamlStream()
    yaml.Load(new StreamReader(filename, Encoding.UTF8))
    schemaFromYamlNode (yaml.Documents.[0].RootNode)
