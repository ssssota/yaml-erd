module Print

open System
open System.IO

open Schema

type private Node =
    { Name: string
      Struct: (Key * Struct) list }

type private Edge =
    { Src: (string * string) list
      Dist: string * string
      Kind: RelationKind * RelationKind }

type private Graphviz = { Nodes: Node list; Edges: Edge list }

let private makeValidLabel (str: string) =
    str.Replace(".", "__").Replace("-", "__")

let private entityToNode (entity: Entity): Node =
    { Name = entity.Name
      Struct = entity.Struct }

let private schemaToGraphviz (schema: Schema.T): Graphviz =
    let nodes = List.map entityToNode schema

    let edges =
        List.fold (fun acc (entity: Entity) ->
            List.fold (fun acc (relation: Relation) ->
                { Src = List.map (fun src -> entity.Name, src) relation.Src
                  Dist = relation.Dist
                  Kind = relation.Kind }
                :: acc) acc entity.Relations) [] schema

    { Nodes = nodes; Edges = edges }

let private printRecord =
    let rec aux indent prefix record =
        List.map (fun (key, strct) ->
            let prefix =
                makeValidLabel
                <| if prefix = "" then key else prefix + "__" + key

            match strct with
            | Scalar v -> String.Format("<{1}>{0}{2}: {3}", indent, prefix, key, v)
            | Record fields ->
                String.Format("<{1}>{0}{2}:\l|{3}", indent, prefix, key, aux (indent + "　") prefix fields)) record
        |> String.concat "\l | "

    aux "" ""

let private printNode (index: int) (node: Node): string =
    String.Format
        ("""  {0} [label="<{0}>{0} | {1}\l" pos="{2},0!"]""", makeValidLabel node.Name, printRecord node.Struct, index)

let private printRelationKind =
    function
    | RelationKind.None -> "none"
    | RelationKind.One -> "teetee"
    | RelationKind.ZeroOrOne -> "teeodot"
    | RelationKind.ZeroOrMore -> "icurveodot"
    | RelationKind.OneOrMore -> "icurvetee"

let mutable private edgeInterCount = 0

let private printEdge edge =
    let arrowhead = printRelationKind <| snd edge.Kind
    let arrowtail = printRelationKind <| fst edge.Kind
    if edge.Src.Length = 1 then
        let src = edge.Src.[0]
        [ String.Format
            ("  {0}:{1} -> {2}:{3} [dir = both, arrowhead = {4}, arrowtail = {5}]",
             makeValidLabel <| fst src,
             makeValidLabel <| snd src,
             makeValidLabel <| fst edge.Dist,
             makeValidLabel <| snd edge.Dist,
             arrowhead,
             arrowtail) ],
        []
    else
        let intermediate =
            String.Format("__intermediate{0}__", edgeInterCount)

        edgeInterCount <- edgeInterCount + 1

        let edges =
            (String.Format
                ("  {0} -> {1}:{2} [dir = forward, arrowhead = {3}]",
                 intermediate,
                 makeValidLabel <| fst edge.Dist,
                 makeValidLabel <| snd edge.Dist,
                 arrowhead))
            :: List.map (fun src ->
                String.Format
                    ("  {0}:{1} -> {2} [dir = back, arrowtail = {3}]",
                     makeValidLabel <| fst src,
                     makeValidLabel <| snd src,
                     intermediate,
                     arrowtail)) edge.Src

        let interNodes =
            String.Format("  {0} [shape=point, width=0.01, height=0.01]", intermediate)

        edges, [ interNodes ]

let private printGraphviz (graphviz: Graphviz): string =
    let nodes =
        List.mapi printNode graphviz.Nodes
        |> String.concat "\n"

    let edges, interNodes =
        List.fold (fun (accEdges, accInterNodes) edge ->
            let edges, interNodes = printEdge edge
            (edges @ accEdges, interNodes @ accInterNodes)) ([], []) graphviz.Edges

    String.Format
        ("""
digraph Schema {{
  rankdir=LR
  graph [
    charset = "UTF-8",
    ranksep = "1.0",
    nodesep = "1.0",
  ]

  node [
    shape = "record",
    fontname = "Noto Sans Mono",
    width = 3
  ]

{0}
{1}

{2}
}}
""",
         nodes,
         interNodes |> String.concat "\n",
         edges |> String.concat "\n")

let schemaToFile (filename: string) (schema: Schema.T) =
    let content = schemaToGraphviz schema |> printGraphviz

    let sw = new StreamWriter(filename)
    sw.Write(content)
    sw.Close()
