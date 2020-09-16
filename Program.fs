open System
open System.Collections.Generic
open System.IO
open System.Text
open System.Text.RegularExpressions
open YamlDotNet.RepresentationModel
open YamlDotNet.Serialization
open YamlDotNet.Serialization.NamingConventions

module DBType =
    type Key = string
    type Struct =
        | Scalar of string
        | Record of (Key * Struct) list

    type Relation = {Src: string; Dist: string * string}

    [<Struct>]
    type Entity = {
        Name: string
        Struct: (Key * Struct) list
        Relations: Relation list
    }

    type Schema = Entity list

    module Parse =
        type OkValue<'a> = {data: 'a; warnings: string list}
        type ErrorValue = string list
        type ParseResult<'a> = Result<OkValue<'a>, ErrorValue>

        let parseError (node: YamlNode) msg =
          Error [
            String.Format(
              "parse error({0}:{1}-{2}:{3}): {4}",
              node.Start.Line,
              node.Start.Column,
              node.End.Line,
              node.End.Column,
              msg
            )
          ]

        let rec structFromYamlNode (node: YamlNode): ParseResult<Struct> =
            match node with
            | :? YamlMappingNode as recordYaml -> recordFromYamlNode recordYaml |> Result.map (fun {data = data; warnings = warnings} -> {data = Record data; warnings = warnings})
            | :? YamlScalarNode as scalarYaml -> scalarFromYamlNode scalarYaml |> Result.map (fun {data = data; warnings = warnings} -> { data = Scalar data; warnings = warnings})
            | _ -> parseError node (String.Format("{0} should be mapping node or scalar node", node.ToString()))

        and recordFromYamlNode (node: YamlNode): ParseResult<(Key * Struct) list> =
            match node with
            | :? YamlMappingNode as recordYaml ->
                Seq.fold (fun acc key ->
                  let key: YamlScalarNode = downcast box key
                  match acc, structFromYamlNode recordYaml.Children.[key] with
                  | Ok {data=data1; warnings=warnings1}, Ok {data=data2; warnings=warnings2} ->
                      Ok {data = (key.ToString(), data2) :: data1; warnings = warnings1 @ warnings2}
                  | Error err1, Error err2 -> Error (err1 @ err2)
                  | _, Error err | Error err, _ -> Error err
                ) (Ok {data = []; warnings = []}) (Seq.rev recordYaml.Children.Keys)
            | _ -> parseError node (String.Format("{0} should be mapping node", node.ToString()))

        and scalarFromYamlNode (node: YamlNode): ParseResult<string> =
            match node with
            | :? YamlScalarNode as scalarYaml -> Ok { data = scalarYaml.Value; warnings = []}
            | _ -> Error [String.Format("parse error({0}:{1}-{2}:{3}):{4} should be scalar node", node.Start.Line, node.Start.Column, node.End.Line, node.End.Column, node.ToString())]

        let relationFromYamlNode (node: YamlNode): ParseResult<Relation> =
          let relationYaml : YamlScalarNode = downcast node
          let regex = Regex("^\\((?<src>.+?)\\) -> (?<distEntity>.+?)\\((?<distField>.+?)\\)$")
          let m = regex.Match(relationYaml.Value)
          Ok {
            data = {
              Src =  m.Groups.["src"].Value
              Dist = m.Groups.["distEntity"].Value, m.Groups.["distField"].Value
            };
            warnings = []
          }

        let entityFromYamlNode (name: string) (node: YamlNode): ParseResult<Entity> =
            let node : YamlMappingNode = downcast node
            let relations =
              if node.Children.ContainsKey(YamlScalarNode("relations")) then
                let relationsYaml : YamlSequenceNode = downcast node.[YamlScalarNode("relations")]
                Seq.fold
                    (fun (acc:ParseResult<Relation list>) (x:YamlNode) ->
                        let relation = relationFromYamlNode x
                        match acc, relation with
                        | Ok {data = rs; warnings = warnings1}, Ok {data=r; warnings=warnings2} ->
                          Ok {
                            data = r :: rs;
                            warnings = warnings1 @ warnings2
                          }
                        | Error err1, Error err2 -> Error (err1 @ err2)
                        | _, Error err | Error err, _ -> Error err
                    )
                    (Ok {data = []; warnings = []})
                    relationsYaml.Children
              else
                Ok {data = []; warnings = []}
            let record = recordFromYamlNode node.[YamlScalarNode("struct")]
            match relations, record with
            | Ok {data = rs; warnings = warnings1}, Ok {data = strct; warnings = warnings2} ->
                Ok {
                  data = {
                    Name = name
                    Struct = strct
                    Relations = rs
                  }
                  warnings = warnings1 @ warnings2
                }
            | Error err1, Error err2 -> Error (err1 @ err2)
            | _, Error err | Error err, _ -> Error err

        let schemaFromYamlNode (node: YamlNode): ParseResult<Schema> =
            let mapping : YamlMappingNode = downcast node
            let schemaYaml : YamlMappingNode = downcast mapping.[YamlScalarNode("schema")]
            Seq.fold (fun acc key ->
                let key : YamlScalarNode = downcast box key
                let entity = entityFromYamlNode key.Value schemaYaml.[key]
                match acc, entity with
                | Ok { data = es; warnings = warnings1 }, Ok { data = e; warnings = warnings2 } ->
                    Ok { data = e :: es; warnings = warnings1 @ warnings2 }
                | Error err1, Error err2 -> Error (err1 @ err2)
                | Error err, _ | _, Error err -> Error err
            ) (Ok {data = []; warnings = []}) schemaYaml.Children.Keys

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
          Src: string * string
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
            let edges: Edge list = List.fold (fun acc (entity:Entity) -> List.fold (fun acc (relation:Relation) -> {Src = entity.Name, relation.Src; Dist = relation.Dist } :: acc) acc entity.Relations) [] schema in
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

        let graphvizToString (graphviz: Graphviz): string =
          let nodes = List.map nodeToString graphviz.Nodes |> String.concat "\n"
          let edgeToString = fun edge -> String.Format("  {0}:{1} -> {2}:{3}", makeValidLabel <| fst edge.Src, makeValidLabel <| snd edge.Src, makeValidLabel <| fst edge.Dist, makeValidLabel <| snd edge.Dist)
          let edges = List.map edgeToString graphviz.Edges |> String.concat "\n"
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
}}
""", nodes, edges)

        let schemaToFile (filename: string) (schema: Schema) =
          let content = schemaToGraphviz schema |> graphvizToString
          let sw = new StreamWriter(filename)
          sw.Write(content)
          sw.Close ()

match DBType.Parse.schemaFromFile @"./sample.yaml" with
| Ok { data = schema; warnings = warnings } ->
    List.iter (fun warning -> Printf.eprintfn "%s" warning) warnings;
    DBType.Print.schemaToFile @"./output.dot" schema
| Error errs ->
  List.iter (fun err -> Printf.eprintfn "%s" err) errs
