module Print

open System.IO
open Util

open Schema
open CalcOrder

type private RelationEdge =
    { Src: EntityName
      Dist: EntityName
      HeadKind: RelationKind
      TailKind: RelationKind
      IsConstraint: bool }

type private LayoutEdge = { Src: EntityName; Dist: EntityName }

let private makeValidLabel (str: string) =
    str
        .Replace(".", "__")
        .Replace("-", "__")
        .Replace("<", "&#12296;")
        .Replace(">", "&#12297;")

let private printRecord =
    let rec aux indent prefix record =
        Seq.map
            (fun ((Key key), strct) ->
                let key = makeValidLabel key

                let prefix =
                    makeValidLabel ((if prefix = "" then key else prefix + "__" + key))

                match strct with
                | Scalar (ScalarVal v, _) ->
                    $"<tr><td port=\"{prefix}\" align=\"left\">{indent}{key}: {makeValidLabel v}</td></tr>"
                | Record (fields, _) ->
                    let after = aux (indent + "　") prefix fields

                    $"    <tr><td port=\"{prefix}\" align=\"left\">{indent}{key}: </td></tr>"
                    + after)
        <| Map.toSeq record
        |> String.concat "\n        "

    aux "" ""

let private printEntity (EntityName entityName, entity: Entity): string =
    let entityName = makeValidLabel entityName
    let record = printRecord entity.Struct

    $"  {entityName} [label=<
    <table border=\"0\" cellborder=\"1\" cellspacing=\"0\">
        <tr><td>{entityName}</td></tr>
        {record}
    </table>>];"

let private printRelationKind =
    function
    | Unknown -> "none"
    | One -> "teetee"
    | ZeroOrOne -> "teeodot"
    | ZeroOrMore -> "icurveodot"
    | OneOrMore -> "icurvetee"

let private calcLayoutEdges (orders: EntityName list list): string * LayoutEdge list =
    let ranks =
        List.map
            (fun order ->
                let order =
                    order
                    |> List.map (fun e -> e.ToString())
                    |> String.concat "; "

                $"  {{rank = same; {order} }}")
            orders
        |> String.concat "\n"

    let maxLength =
        List.fold (fun acc order -> max acc <| List.length order) 0 orders

    let layoutEdges: LayoutEdge list =
        List.fold
            (fun accEdges idx ->
                let nodes =
                    List.fold
                        (fun (accNodes: EntityName list) (order: EntityName list) ->
                            match List.tryNth order idx with
                            | None -> accNodes
                            | Some x -> x :: accNodes)
                        []
                        orders

                if List.length nodes < 2 then
                    accEdges
                else
                    List.fold
                        (fun (accEdges, prev) (node: EntityName) -> ({ Src = prev; Dist = node } :: accEdges), node)
                        (accEdges, nodes.Head)
                        (nodes.Tail)
                    |> fst)
            []
            (List.ofSeq <| seq { 0 .. maxLength })

    ranks, layoutEdges

let private isOrderContained (order: EntityName list list) (src, dist) =
    let rec aux: EntityName list list -> bool =
        function
        | [] -> false
        | (hd :: tl) :: xs ->
            let ok =
                List.fold
                    (fun (prev, acc) (x: EntityName) ->
                        if acc then (x, true)
                        else if prev = src && dist = x then (x, true)
                        else (x, false))
                    (hd, false)
                    tl
                |> snd

            ok || aux xs
        | _ :: xs -> aux xs

    aux order

let private calcEdges layoutEdges (order: EntityName list list) entities =
    List.fold
        (fun (acc, layoutEdges) (entityName, entity) ->
            List.fold
                (fun (acc, layoutEdges) (relation: Relation) ->
                    let dist = entityName
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
    <| Map.toList entities

let private printRelationEdge relationEdge =
    let arrowhead = printRelationKind relationEdge.HeadKind
    let arrowtail = printRelationKind relationEdge.TailKind
    let isConstraint = relationEdge.IsConstraint

    $"  {relationEdge.Src} -> {relationEdge.Dist} [arrowhead = {arrowhead}, arrowtail = {arrowtail}, constraint = {
                                                                                                                       isConstraint
    }, dir = both]"

let private printLayoutEdge layoutEdge =
    $"  {layoutEdge.Src} -> {layoutEdge.Dist} [style = \"invis\"]"

let private printSchema schema =
    let nodes =
        schema.Entities
        |> Map.toList
        |> List.map printEntity
        |> String.concat "\n"

    let order = calcOrder schema
    let (layout, layoutEdges) = calcLayoutEdges order

    let (edges, layoutEdges) =
        calcEdges layoutEdges order schema.Entities

    let layoutEdges =
        layoutEdges
        |> List.map printLayoutEdge
        |> String.concat "\n"

    let edges =
        edges
        |> List.map printRelationEdge
        |> String.concat "\n"

    $"\n
digraph Schema {{
  graph [
    charset = \"UTF-8\"
    newrank = true
    ranksep = 1.5
    nodesep = 1.5
  ];

  node [
    shape = \"none\"
    fontname = \"Noto Sans Mono\"
  ];

{nodes}
{layout}
{layoutEdges}
{edges}
}}
"

let schemaToFile (filename: string) (schema: Schema.T) =
    let content = printSchema schema

    let sw = new StreamWriter(filename)
    sw.Write(content)
    sw.Close()
