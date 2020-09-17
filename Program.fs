open System
open System.Collections.Generic
open System.IO
open System.Text
open System.Text.RegularExpressions
open YamlDotNet.RepresentationModel
open YamlDotNet.Serialization
open YamlDotNet.Serialization.NamingConventions

type Key = string
type Struct =
  | Scalar of string
  | Record of (Key * Struct) list

type Relation = {Src: string list; Dist: string * string}

[<Struct>]
type Entity = {
  Name: string
  Struct: (Key * Struct) list
  Relations: Relation list
}

type Schema = Entity list
type Warning = {
  StartLine: int
  StartColumn: int
  EndLine: int
  EndColumn: int
  Message: string
}

let warningToConsoleString w =
  String.Format("\x1b[33m warning[{0}:{1}-{2}:{3}]: \x1b[0m {4}", w.StartLine, w.StartColumn, w.EndLine, w.EndColumn, w.Message)

type ErrorValue = {
  StartLine: int
  StartColumn: int
  EndLine: int
  EndColumn: int
  Message: string
}

let errorToConsoleString e =
  String.Format("\x1b[31m error[{0}:{1}-{2}:{3}]: \x1b[0m {4}", e.StartLine, e.StartColumn, e.EndLine, e.EndColumn, e.Message)

type OkValue<'a> = {Data: 'a; Warnings: Warning list}
type Result<'a> = Result<OkValue<'a>, ErrorValue list>

module Parse =
  let parseError (node: YamlNode) msg =
    Error [
      {
        StartLine = node.Start.Line
        StartColumn = node.Start.Column
        EndLine = node.End.Line
        EndColumn = node.End.Column
        Message = msg
      }
    ]

  let rec structFromYamlNode (node: YamlNode): Result<Struct> =
    match node with
    | :? YamlMappingNode as recordYaml ->
      recordFromYamlNode recordYaml |> Result.map (fun { Data = data; Warnings = warnings } ->
        { Data = Record data; Warnings = warnings }
      )
    | :? YamlScalarNode as scalarYaml ->
      scalarFromYamlNode scalarYaml |> Result.map (fun { Data = data; Warnings = warnings } ->
        { Data = Scalar data; Warnings = warnings }
      )
    | _ -> parseError node (String.Format("{0} should be mapping node or scalar node", node.ToString()))

  and recordFromYamlNode (node: YamlNode): Result<(Key * Struct) list> =
    match node with
    | :? YamlMappingNode as recordYaml ->
      Seq.fold (fun acc key ->
        let key: YamlScalarNode = downcast box key
        match acc, structFromYamlNode recordYaml.Children.[key] with
        | Ok { Data = data1; Warnings = warnings1 }, Ok { Data = data2; Warnings = warnings2 } ->
          Ok { Data = (key.ToString(), data2) :: data1; Warnings = warnings1 @ warnings2 }
        | Error err1, Error err2 -> Error (err1 @ err2)
        | _, Error err | Error err, _ -> Error err
      ) (Ok { Data = []; Warnings = [] }) (Seq.rev recordYaml.Children.Keys)
    | _ -> parseError node (String.Format("{0} should be mapping node", node.ToString()))

  and scalarFromYamlNode (node: YamlNode): Result<string> =
    match node with
    | :? YamlScalarNode as scalarYaml -> Ok { Data = scalarYaml.Value; Warnings = [] }
    | _ -> parseError node (String.Format("{0} should be scalar node", node.ToString()))

  let relationFromYamlNode (node: YamlNode): Result<Relation> =
    let relationYaml : YamlScalarNode = downcast node
    let regex = Regex("^\\((?<src>.+?)(, (?<srcTail>.+?))*\\) -> (?<distEntity>.+?)\\((?<distField>.+?)\\)$")
    let m = regex.Match(relationYaml.Value)
    if m.Success then
      let srcHead = m.Groups.["src"].Value
      let srcTails = Seq.map (fun (capture:Capture) -> capture.Value) m.Groups.["srcTail"].Captures |> Seq.toList
      Ok {
        Data = {
          Src =  srcHead :: srcTails
          Dist = m.Groups.["distEntity"].Value, m.Groups.["distField"].Value
        }
        Warnings = []
      }
    else
      parseError node (String.Format("`{0}` is not acceptable as relation", node.ToString()))

  let entityFromYamlNode (name: string) (node: YamlNode): Result<Entity> =
    let node : YamlMappingNode = downcast node
    let relations =
      if node.Children.ContainsKey(YamlScalarNode("relations")) then
        let relationsYaml : YamlSequenceNode = downcast node.[YamlScalarNode("relations")]
        Seq.fold
          (fun acc x ->
            let relation = relationFromYamlNode x
            match acc, relation with
            | Ok { Data = rs; Warnings = warnings1 }, Ok { Data = r; Warnings = warnings2 } ->
              Ok {
                Data = r :: rs;
                Warnings = warnings1 @ warnings2
              }
            | Error err1, Error err2 -> Error (err1 @ err2)
            | _, Error err | Error err, _ -> Error err
          )
          (Ok { Data = []; Warnings = [] })
          relationsYaml.Children
      else
        Ok { Data = []; Warnings = [] }
    let record = recordFromYamlNode node.[YamlScalarNode("struct")]
    match relations, record with
    | Ok { Data = rs; Warnings = warnings1 }, Ok { Data = strct; Warnings = warnings2 } ->
      Ok {
        Data = {
          Name = name
          Struct = strct
          Relations = rs
        }
        Warnings = warnings1 @ warnings2
      }
    | Error err1, Error err2 -> Error (err1 @ err2)
    | _, Error err | Error err, _ -> Error err

  let schemaFromYamlNode (node: YamlNode): Result<Schema> =
    let mapping : YamlMappingNode = downcast node
    let schemaYaml : YamlMappingNode = downcast mapping.[YamlScalarNode("schema")]
    Seq.fold (fun acc key ->
      let key : YamlScalarNode = downcast box key
      let entity = entityFromYamlNode key.Value schemaYaml.[key]
      match acc, entity with
        | Ok { Data = es; Warnings = warnings1 }, Ok { Data = e; Warnings = warnings2 } ->
          Ok { Data = e :: es; Warnings = warnings1 @ warnings2 }
        | Error err1, Error err2 -> Error (err1 @ err2)
        | Error err, _ | _, Error err -> Error err
      ) (Ok { Data = []; Warnings = [] }) schemaYaml.Children.Keys

  let schemaFromFile (filename: string) =
    let yaml = YamlStream()
    yaml.Load(new StreamReader(filename, Encoding.UTF8))
    schemaFromYamlNode(yaml.Documents.[0].RootNode)

module Print =
  type Node = {
    Name: string
    Struct: (Key * Struct) list
  }
  type Edge = {
    Src: (string * string) list
    Dist: string * string
  }
  type Graphviz = {
    Nodes: Node list
    Edges: Edge list
  }

  let makeValidLabel (str:string) = str.Replace(".", "__").Replace("-", "__")

  let entityToNode (entity: Entity): Node = {
    Name = entity.Name
    Struct = entity.Struct
  }

  let schemaToGraphviz (schema: Schema): Graphviz =
    let nodes = List.map entityToNode schema
    let edges =
      List.fold
        (fun acc (entity:Entity) ->
          List.fold
            (fun acc (relation:Relation) ->
              {
                Src = List.map (fun src -> entity.Name, src) relation.Src
                Dist = relation.Dist
              } :: acc
            )
            acc
            entity.Relations
        )
        []
        schema in
    {
      Nodes = nodes
      Edges = edges
    }

  let recordToString =
    let rec aux indent prefix record =
      List.map (fun (key, strct) ->
        let prefix = makeValidLabel <| if prefix = "" then key else prefix + "__" + key
        match strct with
        | Scalar v -> String.Format("<{1}>{0}{2}: {3}", indent, prefix, key, v)
        | Record fields -> String.Format("<{1}>{0}{2}:\l|{3}", indent, prefix, key, aux (indent + "　") prefix fields)
      ) record |> String.concat "\l | "
    aux "" ""

  let nodeToString (node: Node): string =
    String.Format("""  {0} [label="<{0}>{0} | {1}\l"]""", makeValidLabel node.Name, recordToString node.Struct)

  let mutable edgeInterCount = 0

  let edgeToString edge =
    if edge.Src.Length = 1 then
      let src = edge.Src.[0]
      [String.Format("  {0}:{1} -> {2}:{3}", makeValidLabel <| fst src, makeValidLabel <| snd src, makeValidLabel <| fst edge.Dist, makeValidLabel <| snd edge.Dist)], []
    else
      let intermediate = String.Format("__intermediate{0}__", edgeInterCount)
      edgeInterCount <- edgeInterCount + 1
      let edges =
        (String.Format("  {0} -> {1}:{2}", intermediate, makeValidLabel <| fst edge.Dist, makeValidLabel <| snd edge.Dist)) ::
        List.map
          (fun src ->
            String.Format("  {0}:{1} -> {2} [dir=none]", makeValidLabel <| fst src, makeValidLabel <| snd src, intermediate)
          )
          edge.Src
      let interNodes = String.Format("  {0} [shape=point, width=0.01, height=0.01]", intermediate)
      edges, [interNodes]

  let graphvizToString (graphviz: Graphviz): string =
    let nodes = List.map nodeToString graphviz.Nodes |> String.concat "\n"
    let edges, interNodes =
      List.fold
        (fun (accEdges, accInterNodes) edge ->
          let (edges, interNodes) = edgeToString edge
          (edges @ accEdges, interNodes @ accInterNodes)
        ) ([], []) graphviz.Edges
    String.Format("""
digraph Schema {{
  rankdir=LR
  graph [
    charset = "UTF-8";
  ]

  node [
    shape = "record"
    fontname = "Noto Sans Mono"
    width = 3
  ]

{0}
{1}

{2}
}}
""", nodes, interNodes |> String.concat "\n", edges |> String.concat "\n")

  let schemaToFile (filename: string) (schema: Schema) =
    let content = schemaToGraphviz schema |> graphvizToString
    let sw = new StreamWriter(filename)
    sw.Write(content)
    sw.Close ()

module Validation =
  let validate x = x // TODO

match Parse.schemaFromFile @"./sample.yaml" with
| Ok { Data = schema; Warnings = warnings } ->
    List.iter (fun warning -> Printf.eprintfn "%s" <| warningToConsoleString warning) warnings;
    Print.schemaToFile @"./output.dot" schema
| Error errs ->
  List.iter (fun err -> Printf.eprintfn "%s" <| errorToConsoleString err) errs
