module App

open Elmish
open Elmish.React
open Feliz
open Feliz.Router

module Root = 
  [<RequireQualifiedAccess>]
  type Url = 
    | MainPage
    | RestrictedPage
    | NotFound

  let whichPage = function 
  | Url.MainPage       -> "data main"
  | Url.RestrictedPage -> "data restricted"
  | Url.NotFound       -> "error"

  let parseUrl = function
  | [ ] -> Url.MainPage
  | [ "restricted" ] -> Url.RestrictedPage
  | _ -> Url.NotFound

  type State =
    { CurrentUrl: Url }

  type Msg =
    | UrlChanged of Url

  let init() =
    let startingUrl = Router.currentUrl() |> parseUrl
    match startingUrl with
    | Url.MainPage  -> { CurrentUrl = startingUrl }, Cmd.none
    | Url.RestrictedPage  -> { CurrentUrl = startingUrl }, Router.navigate("/", HistoryMode.ReplaceState)
    | Url.NotFound  -> { CurrentUrl = startingUrl }, Cmd.none

  let update (msg: Msg) (state: State) =
    match msg with
    | UrlChanged newUrl -> { state with CurrentUrl = newUrl }, Cmd.none

  let render (state: State) (dispatch: Msg -> unit) =
    let activePage = 
      match state.CurrentUrl with
      | Url.MainPage       -> Html.h1 (sprintf "main page - %s" (whichPage state.CurrentUrl))
      | Url.RestrictedPage -> Html.h1 (sprintf "restricted page - %s" (whichPage state.CurrentUrl))
      | Url.NotFound       -> Html.h1 "not found page"

    Router.router [
      Router.onUrlChanged (parseUrl >> UrlChanged >> dispatch)
      Router.application [
        activePage
      ]
    ]

Program.mkProgram Root.init Root.update Root.render
|> Program.withReactSynchronous "elmish-app"
|> Program.run