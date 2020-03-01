module Output.QuizOutput exposing (..)

import Browser
import Common.Constants exposing (getQuizRatingsApi)
import Common.Types exposing (DbQuizId, jsonDecQuizRatings)
import Common.Util exposing (getMsg)
import Output.Model as Model exposing (Model(..), Msg(..), initialModelFunction)
import Output.Views exposing (view)


main : Program () Model Msg
main =
    Browser.document
        { init = initialModelFunction
        , view = \model -> { title = Model.titleFor model, body = [ view model ] }
        , update = update
        , subscriptions = \_ -> Sub.none
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( GetQuizRatings qid, AllModel _ _ ) ->
            ( model, getQuizRatings qid )

        _ ->
            ( model, Cmd.none )


getQuizRatings : DbQuizId -> Cmd Msg
getQuizRatings =
    getMsg getQuizRatingsApi GotQuizRatings jsonDecQuizRatings
