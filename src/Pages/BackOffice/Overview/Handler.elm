module Pages.BackOffice.Overview.Handler exposing (init, update)

import Api.Api
import Pages.BackOffice.Overview.Page as Page
import Result.Extra
import Util.Tristate as Tristate


init : ( Page.Model, Cmd Page.Msg )
init =
    ( { quizzes = Tristate.initial
      }
    , Api.Api.public { toMsg = Page.GotQuizzes }
    )


update : Page.Msg -> Page.Model -> ( Page.Model, Cmd Page.Msg )
update msg model =
    case msg of
        Page.GotQuizzes result ->
            ( Page.lenses.quizzes.set (Result.Extra.unpack Tristate.failed Tristate.ready result) model
            , Cmd.none
            )
