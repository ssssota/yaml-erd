open System
open System.Collections.Generic
open System.IO
open System.Text
open System.Text.RegularExpressions
open YamlDotNet.RepresentationModel
open YamlDotNet.Serialization
open YamlDotNet.Serialization.NamingConventions

module DBType =
    type Key = String
    type Struct =
        | Scalar of String
        | Record of (Key * Struct) list

    type Relation = String * String

    [<Struct>]
    type Table = {
        Name: String
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
          let regex = Regex("^\\((?<from>.+?)\\) -> (?<to>.+?)$")
          let m = regex.Match(relationYaml.Value)
          Relation(m.Groups.["from"].Value, m.Groups.["to"].Value)

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
          From: string
          To: string
        }
        type Graphviz = {
          Nodes: Node list
          Edges: Edge list
        }

        let tableToNode (table: Table): Node = {
          Name = table.Name
          Struct = table.Struct
        }

        let schemaToGraphviz (schema: Schema): Graphviz =
            let nodes = List.map tableToNode schema
            let edges = [] // TODO
            {
              Nodes = nodes
              Edges = edges
            }

        let recordToString =
          let rec aux indent record =
            List.map (fun (key, strct) ->
              match strct with
              | Scalar v -> indent + key + ": " + v
              | Record fields -> indent + key + ":\l |" + (aux (indent + "　") fields)
            ) record |> String.concat "\l | "
          aux ""

        let nodeToString (node: Node): String =
          String.Format("""{0} [label="{0} | {1}\l"]""", node.Name, recordToString node.Struct)

        let graphvizToString (graphviz: Graphviz): string =
          let nodes = List.map nodeToString graphviz.Nodes |> String.concat "\n"
          let edges = List.map (fun edge -> edge.From + " -> " + edge.To) graphviz.Edges |> String.concat "\n"
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
printfn "input: %A" schema
DBType.Print.schemaToFile @"./output.dot" schema
