module Print

open System
open System.IO
open Util

open Schema
open CalcOrder

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

let private printEntity (entity: Entity): string =
    String.Format("""  {0} [label=<
    <table border="0" cellborder="1" cellspacing="0">
        <tr><td>{0}</td></tr>
        {1}
    </table>>];""", makeValidLabel entity.Name, printRecord entity.Struct)

let private printRelationKind =
    function
    | Unknown -> "none"
    | One -> "teetee"
    | ZeroOrOne -> "teeodot"
    | ZeroOrMore -> "icurveodot"
    | OneOrMore -> "icurvetee"

let mutable private edgeInterCount = 0

let private printEdges edges =
    let printEdge (accEdges, accInterNodes) edge =
        let arrowhead = printRelationKind <| snd edge.Kind
        let arrowtail = printRelationKind <| fst edge.Kind
        if edge.Src.Length = 1 then
            let src = edge.Src.[0]
            let newEdge =
                String.Format(
                    "  {0}:{1} -> {2}:{3} [arrowhead = {4}, arrowtail = {5}, constraint = false, dir = both];",
                    makeValidLabel <| fst src,
                    makeValidLabel <| snd src,
                    makeValidLabel <| fst edge.Dist,
                    makeValidLabel <| snd edge.Dist,
                    arrowhead,
                    arrowtail)
            newEdge :: accEdges, accInterNodes
        else
            let intermediate =
                String.Format("__intermediate{0}__", edgeInterCount)

            edgeInterCount <- edgeInterCount + 1
            let firstHalfEdge =
                String.Format(
                    "  {0} -> {1}:{2} [arrowhead = {3}, constraint = false, dir = forward];",
                    intermediate,
                    makeValidLabel <| fst edge.Dist,
                    makeValidLabel <| snd edge.Dist,
                    arrowhead)
            let edges = firstHalfEdge :: List.fold (fun acc src ->
                String.Format(
                    "  {0}:{1} -> {2} [arrowtail = {3}, constraint = false, dir = back];",
                    makeValidLabel <| fst src,
                    makeValidLabel <| snd src,
                    intermediate,
                    arrowtail) :: acc) accEdges edge.Src

            let interNode =
                String.Format("  {0} [shape = point, width = 0.01 height = 0.01];", intermediate)

            edges, interNode :: accInterNodes
    List.fold printEdge ([], []) edges


let private printLayout (orders: string list list): string =
    let ranks = List.map (fun order -> String.Format("""  {{rank = same; {0} }}""", String.concat "; " order)) orders |> String.concat "\n"
    let lineEdges =
        List.map (fun order ->
            if List.length order < 2 then ""
            else String.Format("""  {0} [style = "invis"]""", String.concat " -> " order)
        ) orders |> String.concat "\n"
    let maxLength = List.fold (fun acc order -> max acc <| List.length order) 0 orders
    let dummyEdge: string list =
        List.map (fun idx ->
            let nodes =
                List.fold (fun (acc: string list) (order: string list) ->
                    match List.tryNth order idx with
                    | None -> acc
                    | Some x -> x :: acc) [] orders
            if List.length nodes < 2 then ""
            else
                let edge: string = nodes |> String.concat " -> "
                String.Format("""  {0} [style = "invis"]""", edge)
        ) (List.ofSeq <| seq{ 0..maxLength })

    ranks + "\n" + lineEdges + "\n" + (String.concat "\n" dummyEdge)

let private printSchema schema: string =
    let nodes = List.map printEntity schema |> String.concat "\n"
    let edges = List.fold (fun acc (entity: Entity) ->
            List.fold (fun acc (relation: Relation) ->
                { Src = List.map (fun src -> entity.Name, src) relation.Src
                  Dist = relation.Dist
                  Kind = relation.Kind }
                :: acc) acc entity.Relations) [] schema
    let edges, interNodes = printEdges edges
    let order = calcOrder schema
    let layout = printLayout order

    String.Format
        ("""
digraph Schema {{
  graph [
    charset = "UTF-8"
    newrank = true
    ranksep = 1.5
    nodesep = 1.5
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
         layout,
         edges |> String.concat "\n")

let schemaToFile (filename: string) schema =
    let content = printSchema schema

    let sw = new StreamWriter(filename)
    sw.Write(content)
    sw.Close()
