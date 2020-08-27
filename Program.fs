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
        Struct: Struct
        Relations: Relation list
    }

    type Schema = Table list

    module Parse =
        let rec structFromYamlNode (node: YamlNode) : Struct =
            match node with
            | :? YamlMappingNode as recordYaml ->
                Record(List.map (fun key ->
                  let key : YamlScalarNode = downcast box key
                  (key.Value, structFromYamlNode <| recordYaml.Children.[key])
                ) <| Seq.toList recordYaml.Children.Keys)
            | :? YamlScalarNode as scalarYaml -> Scalar(scalarYaml.Value)
            | _ -> failwith "unreachable!!"

        let relationFromYamlNode (node: YamlNode) : Relation =
          let relationYaml : YamlScalarNode = downcast node
          let regex = Regex("^\\((?<from>.+?)\\) -> (?<to>.+?)$")
          let m = regex.Match(relationYaml.Value)
          Relation(m.Groups.["from"].Value, m.Groups.["to"].Value)

        let tableFromYamlNode (name:string, node: YamlNode) : Table =
            let node : YamlMappingNode = downcast node
            let relations =
              if node.Children.ContainsKey(YamlScalarNode("relations")) then
                let relationsYaml : YamlSequenceNode = downcast node.[YamlScalarNode("relations")]
                List.map relationFromYamlNode (List.ofSeq relationsYaml.Children)
              else
              []
            {
                Name = name
                Struct = structFromYamlNode node.[YamlScalarNode("struct")]
                Relations = relations
            }

        let schemaFromYamlNode (node:YamlNode) : Schema =
            let mapping : YamlMappingNode = downcast node
            let schemaYaml : YamlMappingNode = downcast mapping.[YamlScalarNode("schema")]
            List.map (fun key ->
              let key : YamlScalarNode = downcast box key
              tableFromYamlNode(key.Value, schemaYaml.[key])
            ) <| Seq.toList schemaYaml.Children.Keys

        let schemaFromFile (filename:string) =
          let yaml = YamlStream()
          yaml.Load(new StreamReader(filename, Encoding.UTF8))
          schemaFromYamlNode(yaml.Documents.[0].RootNode)

printfn "%A"  <| DBType.Parse.schemaFromFile "./sample.yaml"
