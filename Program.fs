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
    type Table = {
        Name: string
        Struct: (Key * Struct) list
        Relations: Relation list
    }

    type Schema = Table list

    module Parse =
        let rec structFromYamlNode (node: YamlNode): Struct =
            match node with
            | :? YamlMappingNode as recordYaml ->
                Record (List.map (fun key ->
                  let key : YamlScalarNode = downcast box key
                  (key.Value, structFromYamlNode <| recordYaml.Children.[key]))
                <| Seq.toList recordYaml.Children.Keys)
            | :? YamlScalarNode as scalarYaml -> Scalar scalarYaml.Value
            | _ -> failwith "unreachable!!"

        let recordFromYamlNode (node: YamlNode): (Key * Struct) list =
            match structFromYamlNode node with
            | Record fields -> fields
            | _ -> failwith "unreachable!!"

        let relationFromYamlNode (node: YamlNode): Relation =
          let relationYaml : YamlScalarNode = downcast node
          let regex = Regex("^\\((?<src>.+?)\\) -> (?<distEntity>.+?)\\((?<distField>.+?)\\)$")
          let m = regex.Match(relationYaml.Value)
          {
            Src =  m.Groups.["src"].Value
            Dist = m.Groups.["distEntity"].Value, m.Groups.["distField"].Value
          }

        let tableFromYamlNode (name: string) (node: YamlNode): Table =
            let node : YamlMappingNode = downcast node
            let relations =
              if node.Children.ContainsKey(YamlScalarNode("relations")) then
                let relationsYaml : YamlSequenceNode = downcast node.[YamlScalarNode("relations")]
                List.map relationFromYamlNode (List.ofSeq relationsYaml.Children)
              else
                []
            {
                Name = name
                Struct = recordFromYamlNode node.[YamlScalarNode("struct")]
                Relations = relations
            }

        let schemaFromYamlNode (node: YamlNode): Schema =
            let mapping : YamlMappingNode = downcast node
            let schemaYaml : YamlMappingNode = downcast mapping.[YamlScalarNode("schema")]
            List.map (fun key ->
              let key : YamlScalarNode = downcast box key
              tableFromYamlNode key.Value schemaYaml.[key]
            ) <| Seq.toList schemaYaml.Children.Keys

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

        let tableToNode (table: Table): Node = {
          Name = table.Name
          Struct = table.Struct
        }

        let schemaToGraphviz (schema: Schema): Graphviz =
            let nodes = List.map tableToNode schema
            let edges: Edge list = List.fold (fun acc (table:Table) -> List.fold (fun acc (relation:Relation) -> {Src = table.Name, relation.Src; Dist = relation.Dist } :: acc) acc table.Relations) [] schema in
            {
              Nodes = nodes
              Edges = edges
            }

        let recordToString =
          let rec aux indent prefix record =
            List.map (fun (key: string, strct) ->
              let prefix = makeValidLabel <| if prefix = "" then key else prefix + "__" + key
              match strct with
              | Scalar v -> String.Format("<{1}>{0}{2}: {3}", indent, prefix, key, v)
              | Record fields -> String.Format("<{1}>{0}{2}:\l|{3}", indent, prefix, key, aux (indent + "　") prefix fields)
            ) record |> String.concat "\l | "
          aux "" ""

        let nodeToString (node: Node): string =
          String.Format("""{0} [label="<{0}>{0} | {1}\l"]""", makeValidLabel node.Name, recordToString node.Struct)

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

let schema = DBType.Parse.schemaFromFile @"./sample.yaml"
DBType.Print.schemaToFile @"./output.dot" schema
