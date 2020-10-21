module Print

open System
open System.IO
open Util

open Schema

type private Edge =
    { Src: (string * string) list
      Dist: string * string
      Kind: RelationKind * RelationKind }

let private makeValidLabel (str: string) =
    str.Replace(".", "__").Replace("-", "__")

let private printRecord =
    let rec aux indent prefix record =
        List.map (fun (key, strct) ->
            let prefix =
                makeValidLabel
                <| if prefix = "" then key else prefix + "__" + key

            match strct with
            | Scalar v -> String.Format("""<tr><td port="{1}" align="left">{0}{2}: {3}</td></tr>""", indent, prefix, key, v)
            | Record fields ->
                let after = aux (indent + "　") prefix fields
                String.Format("""    <tr><td port="{1}" align="left">{0}{2}: </td></tr>""", indent, prefix, key) + after) record
        |> String.concat "\n        "

    aux "" ""

let private printEntityRank (entities: Entity list): string =
    if entities.IsEmpty then ""
    else
        let nodes = List.fold (fun acc entity -> acc + "\n" + String.Format("""  {0} [label=<
    <table border="0" cellborder="1" cellspacing="0">
        <tr><td>{0}</td></tr>
        {1}
    </table>>];""", makeValidLabel entity.Name, printRecord entity.Struct)) "" entities
        let rank = String.Format("""  {{rank = same; {0} }}""", List.map (fun entity -> makeValidLabel entity.Name) entities |> String.concat "; ")
        nodes + "\n" + rank

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
            ("  {0}:{1} -> {2}:{3} [arrowhead = {4}, arrowtail = {5}, constraint = false, dir = both];",
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
                ("  {0} -> {1}:{2} [arrowhead = {3}, constraint = false, dir = forward];",
                 intermediate,
                 makeValidLabel <| fst edge.Dist,
                 makeValidLabel <| snd edge.Dist,
                 arrowhead))
            :: List.map (fun src ->
                String.Format
                    ("  {0}:{1} -> {2} [arrowtail = {3}, constraint = false, dir = back];",
                     makeValidLabel <| fst src,
                     makeValidLabel <| snd src,
                     intermediate,
                     arrowtail)) edge.Src

        let interNodes =
            String.Format("  {0} [shape = point, width = 0.01 height = 0.01];", intermediate)

        edges, [ interNodes ]

let private printSchema (entityRanks: Entity list list) (edges: Edge list) : string =
    let nodes = List.map printEntityRank entityRanks |> String.concat "\n"
    let dummyEdge = String.concat " -> " <| List.map (fun entities -> if List.isEmpty entities then "" else (List.head entities).Name) entityRanks 
    let dummyEdge = String.Format("""  {0} [style = "invis"]""", dummyEdge)

    let edges, interNodes =
        List.fold (fun (accEdges, accInterNodes) edge ->
            let edges, interNodes = printEdge edge
            (edges @ accEdges, interNodes @ accInterNodes)) ([], []) edges

    String.Format
        ("""
digraph Schema {{
  graph [
    charset = "UTF-8"
    newrank = true
  ];

  node [
    shape = "none"
    fontname = "Noto Sans Mono"
  ];

{0}
{1}
{2}

{3}
}}
""",
         nodes,
         interNodes |> String.concat "\n",
         dummyEdge,
         edges |> String.concat "\n")

let schemaToFile (filename: string) (rankedEntities: Entity list list) =
    let calcEdges =
        List.fold (fun acc (entity: Entity) ->
            List.fold (fun acc (relation: Relation) ->
                { Src = List.map (fun src -> entity.Name, src) relation.Src
                  Dist = relation.Dist
                  Kind = relation.Kind }
                :: acc) acc entity.Relations)
    let edges = List.fold calcEdges [] rankedEntities
    let content = printSchema rankedEntities edges

    let sw = new StreamWriter(filename)
    sw.Write(content)
    sw.Close()
