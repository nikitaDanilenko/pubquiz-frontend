module Output.QuizOutput exposing (..)

import Browser
import Output.Model exposing (Model, Msg, initialModelFunction)
import Output.Views exposing (view)

main : Program () Model Msg
main =
    Browser.document
        { init = initialModelFunction
        , view = \model -> { title = "Quiz Interface", body = [ view model ] }
        , update = update
        , subscriptions = \_ -> Sub.none
        }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = (model, Cmd.none)