module YamlErdOnWeb.Client.Main

open Elmish
open Bolero
open Bolero.Html

open Microsoft.JSInterop

type Model =
    { input: string
      output: string
      errors: string }

let initModel = { input = "schema:\n"; output = ""; errors = "" }

type Message =
    | Input of string
    | Convert

let update (js: IJSRuntime) msg model =
    match msg with
    | Input src -> { model with input = src }
    | Convert ->
        match Parse.schemaFromString model.input with
        | Ok { Data = schema; Warnings = warnings } ->
            match Validate.validate schema with
            | Ok { Data = schema; Warnings = warnings } ->
                let dot = Print.schemaToString schema
                js.InvokeVoidAsync("showOutput", dot) |> ignore // todo
                { model with output = dot; errors = "" }
            | Error errs ->
                let errs =
                    errs
                    |> Array.map (fun err -> err.ToString())
                    |> String.concat "\n"

                { model with errors = errs }
        | Error errs ->
            let errs =
                errs
                |> Array.map (fun err -> err.ToString())
                |> String.concat "\n"

            { model with errors = errs }


let view model dispatch =
    div [] [
        label [ attr.classes [ "label" ] ] [
            text "input:"
        ]
        textarea [ attr.classes [ "textarea" ]
                   attr.rows 12
                   bind.input.string model.input (fun v -> dispatch (Input v); dispatch Convert) ] []
        button [ attr.classes [ "button" ]
                 on.click (fun _ -> dispatch Convert) ] [
            text "convert!"
        ]
        label [ attr.classes [ "label" ] ] [
            text "output:"
            textarea [ attr.classes [ "textarea" ] ] [
                text model.output
            ]
        ]
        label [ attr.classes [ "label" ] ] [
            text "errors:"
            textarea [ attr.classes [ "textarea" ] ] [
                text model.errors
            ]
        ]
    ]

type MyApp() =
    inherit ProgramComponent<Model, Message>()

    override this.Program =
        Program.mkSimple (fun _ -> initModel) (update this.JSRuntime) view
