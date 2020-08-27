module App

open Elmish
open Elmish.React
open Fable.React
open Fable.React.Props
open Fable.SimpleHttp

[<Literal>]
let path =__SOURCE_DIRECTORY__ + "/selection.json"
type Icons = Fable.IcoMoonProvider.Generator<path>

type Model =
  { Name: string }

type Msg =
  | No

let init() : Model * Cmd<Msg> =
  { Name = "" }, Cmd.none

let update (msg:Msg) (model:Model) =
    match msg with
    | No -> model, Cmd.none


let view (model:Model) dispatch =
  div [ Style [ Display DisplayOptions.Flex
                FlexDirection "row"  ] ]
    [
      str Icons.home2
    ]

// App
Program.mkProgram init update view
|> Program.withReactSynchronous "elmish-app"
|> Program.withConsoleTrace
|> Program.run
