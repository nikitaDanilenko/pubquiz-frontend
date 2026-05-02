module Pages.Public.Overview.Handler exposing (init, update)

{-| Quiz overview page logic.
-}

import Api.Json
import Http
import Json.Decode
import Pages.Public.Overview.Page as Page
import Result.Extra
import Util.Tristate as Tristate


init : Page.Flags -> ( Page.Model, Cmd Page.Msg )
init flags =
    ( { quizzes = Tristate.initial
      , searchText = ""
      , sortBy = Page.ByDate
      , sortDirection = Page.Descending
      }
    , fetchQuizzes flags.apiBase
    )


update : Page.Msg -> Page.Model -> ( Page.Model, Cmd Page.Msg )
update msg model =
    case msg of
        Page.GotQuizzes result ->
            ( Page.lenses.quizzes.set (Result.Extra.unpack Tristate.failed Tristate.ready result) model
            , Cmd.none
            )

        Page.SetSearchText text ->
            ( Page.lenses.searchText.set text model
            , Cmd.none
            )

        Page.SetSortBy sortBy ->
            ( Page.lenses.sortBy.set sortBy model
            , Cmd.none
            )

        Page.SetSortDirection direction ->
            ( Page.lenses.sortDirection.set direction model
            , Cmd.none
            )


fetchQuizzes : String -> Cmd Page.Msg
fetchQuizzes apiBase =
    Http.get
        { url = String.concat [ apiBase, "/public" ]
        , expect = Http.expectJson Page.GotQuizzes (Json.Decode.list Api.Json.decodeQuizSummary)
        }
