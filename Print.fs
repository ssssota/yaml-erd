module Print

open System
open System.IO
open Util

open Schema
open CalcOrder

type private RelationEdge =
    { Src: string
      Dist: string
      HeadKind: RelationKind
      TailKind: RelationKind
      IsConstraint: bool }

type private LayoutEdge = { Src: string; Dist: string }

let private makeValidLabel (str: string) =
    str
        .Replace(".", "__")
        .Replace("-", "__")
        .Replace("<", "&#12296;")
        .Replace(">", "&#12297;")

let private printRecord =
    let rec aux indent prefix record =
        List.map
            (fun (key, strct) ->
                let key = makeValidLabel key

                let prefix =
                    makeValidLabel
                    <| if prefix = "" then key else prefix + "__" + key

                match strct with
                | Scalar (v, _) ->
                    String.Format(
                        """<tr><td port="{1}" align="left">{0}{2}: {3}</td></tr>""",
                        indent,
                        prefix,
                        key,
                        makeValidLabel v
                    )
                | Record (fields, _) ->
                    let after = aux (indent + "　") prefix fields

                    String.Format("""    <tr><td port="{1}" align="left">{0}{2}: </td></tr>""", indent, prefix, key)
                    + after)
            record
        |> String.concat "\n        "

    aux "" ""

let private printEntity (entity: Entity): string =
    String.Format(
        """  {0} [label=<
    <table border="0" cellborder="1" cellspacing="0">
        <tr><td>{0}</td></tr>
        {1}
    </table>>];""",
        makeValidLabel entity.Name,
        printRecord entity.Struct
    )

let private printRelationKind =
    function
    | Unknown -> "none"
    | One -> "teetee"
    | ZeroOrOne -> "teeodot"
    | ZeroOrMore -> "icurveodot"
    | OneOrMore -> "icurvetee"

let private calcLayoutEdges (orders: string list list): string * LayoutEdge list =
    let ranks =
        List.map (fun order -> String.Format("""  {{rank = same; {0} }}""", String.concat "; " order)) orders
        |> String.concat "\n"

    let maxLength =
        List.fold (fun acc order -> max acc <| List.length order) 0 orders

    let layoutEdges: LayoutEdge list =
        List.fold
            (fun accEdges idx ->
                let nodes =
                    List.fold
                        (fun (accNodes: string list) (order: string list) ->
                            match List.tryNth order idx with
                            | None -> accNodes
                            | Some x -> x :: accNodes)
                        []
                        orders

                if List.length nodes < 2 then
                    accEdges
                else
                    List.fold
                        (fun (accEdges, prev) (node: string) -> ({ Src = prev; Dist = node } :: accEdges), node)
                        (accEdges, nodes.Head)
                        (nodes.Tail)
                    |> fst)
            []
            (List.ofSeq <| seq { 0 .. maxLength })

    ranks, layoutEdges

let private isOrderContained (order: string list list) (src, dist) =
    let rec aux: string list list -> bool = function
    | [] -> false
    | (hd :: tl) :: xs ->
        let ok =
            List.fold
                (fun (prev, acc) (x: string) ->
                   if acc then (x, true)
                   else if  prev = src && dist = x then (x, true)
                   else (x, false)
                )
                (hd, false)
                tl
            |> snd
        ok || aux xs
    | _ :: xs -> aux xs
    aux order

let private calcEdges layoutEdges (order: string list list) schema =
    List.fold
        (fun (acc, layoutEdges) (entity: Entity) ->
            List.fold
                (fun (acc, layoutEdges) (relation: Relation) ->
                    let dist = entity.Name
                    let src = fst relation.Dist

                    let existsLayout =
                        fun (layoutEdge: LayoutEdge) ->
                            layoutEdge.Src = src && layoutEdge.Dist = dist
                            || layoutEdge.Src = dist && layoutEdge.Dist = src

                    match List.findPop existsLayout layoutEdges with
                    | None, layoutEdges ->
                        let newEdge =
                            { Src = src
                              Dist = dist
                              HeadKind = fst relation.Kind
                              TailKind = snd relation.Kind
                              IsConstraint = isOrderContained order (src, dist) }

                        newEdge :: acc, layoutEdges
                    | Some (layoutEdge), layoutEdges ->
                        if layoutEdge.Src = src then
                            { Src = src
                              Dist = dist
                              HeadKind = fst relation.Kind
                              TailKind = snd relation.Kind
                              IsConstraint = true }
                            :: acc,
                            layoutEdges
                        else // reversed
                            { Src = dist
                              Dist = src
                              HeadKind = snd relation.Kind
                              TailKind = fst relation.Kind
                              IsConstraint = true }
                            :: acc,
                            layoutEdges)
                (acc, layoutEdges)
                entity.Relations)
        ([], layoutEdges)
        schema

let private printRelationEdge relationEdge =
    let arrowhead = printRelationKind relationEdge.HeadKind
    let arrowtail = printRelationKind relationEdge.TailKind

    String.Format(
        """  {0} -> {1} [arrowhead = {2}, arrowtail = {3}, constraint = {4}, dir = both]""",
        relationEdge.Src,
        relationEdge.Dist,
        arrowhead,
        arrowtail,
        if relationEdge.IsConstraint then "true" else "false"
    )

let private printLayoutEdge layoutEdge =
    String.Format("""  {0} -> {1} [style = "invis"]""", layoutEdge.Src, layoutEdge.Dist)

let private printSchema schema entitySets: string =
    let nodes =
        List.map printEntity schema |> String.concat "\n"

    let order = calcOrder schema entitySets
    let (layout, layoutEdges) = calcLayoutEdges order
    let (edges, layoutEdges) = calcEdges layoutEdges order schema

    String.Format(
        """
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
        layout,
        layoutEdges
        |> List.map printLayoutEdge
        |> String.concat "\n",
        edges
        |> List.map printRelationEdge
        |> String.concat "\n"
    )

let schemaToFile (filename: string) (schema: Schema.T) (entitySets: string [] list) =
    let content = printSchema schema entitySets

    let sw = new StreamWriter(filename)
    sw.Write(content)
    sw.Close()
