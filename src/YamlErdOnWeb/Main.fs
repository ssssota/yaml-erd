module YamlErdOnWeb.Client.Main

open Elmish
open Bolero
open Bolero.Html



type Model = { input: string; output: string }

let initModel = { input = ""; output = "" }

type Message =
    | Input of string
    | Convert

let update message model =
    match message with
    | Input src -> { model with input = src }
    | Convert -> { model with output = model.input }

let view model dispatch =
    div [] [
        textarea [ bind.input.string model.input (fun v -> dispatch (Input v)) ] []
        button [ on.click (fun _ -> dispatch Convert) ] [
            text "convert!"
        ]
        text <| model.output
    ]

type MyApp() =
    inherit ProgramComponent<Model, Message>()

    override this.Program =
        Program.mkSimple (fun _ -> initModel) update view
