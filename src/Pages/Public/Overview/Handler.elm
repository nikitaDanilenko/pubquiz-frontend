module Pages.Public.Overview.Handler exposing (init, update)

{-| Quiz overview page logic.
-}

import Api.Api
import Pages.Public.Overview.Page as Page
import Result.Extra
import Util.Tristate as Tristate


init : ( Page.Model, Cmd Page.Msg )
init =
    ( { quizzes = Tristate.initial
      , searchText = ""
      , sortBy = Page.ByDate
      , sortDirection = Page.Descending
      }
    , fetchQuizzes
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


fetchQuizzes : Cmd Page.Msg
fetchQuizzes =
    Api.Api.public { toMsg = Page.GotQuizzes }
