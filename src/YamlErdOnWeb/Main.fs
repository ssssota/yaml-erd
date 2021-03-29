module YamlErdOnWeb.Client.Main

open Elmish
open Bolero
open Bolero.Html



type Model = { input: string; output: string }

let initModel = { input = ""; output = "" }

type Message =
    | Input of string
    | Convert


let private convert input =
  match Parse.schemaFromString input with
  | Ok { Data = schema; Warnings = warnings } ->
    let warnings = warnings |> Array.map (fun w -> w.ToString()) |> String.concat "\n"
    warnings +
      match Validate.validate schema with
      | Ok { Data = schema; Warnings = warnings} ->
        Print.schemaToString schema
      | Error errs ->
        errs |> Array.map (fun err -> err.ToString()) |> String.concat "\n"
  | Error errs ->
    errs |> Array.map (fun err -> err.ToString()) |> String.concat "\n"

let update message model =
    match message with
    | Input src -> { model with input = src }
    | Convert -> { model with output = convert model.input }

let view model dispatch =
    div [] [
        textarea [attr.rows 12;  bind.input.string model.input (fun v -> dispatch (Input v)) ] []
        button [ on.click (fun _ -> dispatch Convert) ] [
            text "convert!"
        ]
        text <| model.output
    ]

type MyApp() =
    inherit ProgramComponent<Model, Message>()

    override this.Program =
        Program.mkSimple (fun _ -> initModel) update view
