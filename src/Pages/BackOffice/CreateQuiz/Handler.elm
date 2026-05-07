module Pages.BackOffice.CreateQuiz.Handler exposing (init, update)

{-| Create Quiz page logic.
-}

import Api.Api
import Api.Types exposing (QuizIdentifier, QuizMetaData, QuizSettings)
import Date
import OpenApi.Common
import Pages.BackOffice.CreateQuiz.Page as Page


defaultQuestionsPerRound : Int
defaultQuestionsPerRound =
    8


defaultNumberOfRounds : Int
defaultNumberOfRounds =
    4


defaultNumberOfTeams : Int
defaultNumberOfTeams =
    8


init : ( Page.Model, Cmd Page.Msg )
init =
    ( { name = ""
      , date = ""
      , place = ""
      , numberOfRounds = defaultNumberOfRounds
      , questionsPerRound = List.repeat defaultNumberOfRounds defaultQuestionsPerRound
      , numberOfTeams = defaultNumberOfTeams
      , isSubmitting = False
      , error = Nothing
      }
    , Cmd.none
    )


update : Page.Msg -> Page.Model -> ( Page.Model, Cmd Page.Msg, Maybe Int )
update msg model =
    case msg of
        Page.SetName name ->
            ( Page.lenses.name.set name model
            , Cmd.none
            , Nothing
            )

        Page.SetDate date ->
            ( Page.lenses.date.set date model
            , Cmd.none
            , Nothing
            )

        Page.SetPlace place ->
            ( Page.lenses.place.set place model
            , Cmd.none
            , Nothing
            )

        Page.SetNumberOfRounds input ->
            let
                newRounds =
                    String.toInt input
                        |> Maybe.withDefault model.numberOfRounds
                        |> max 1

                currentLength =
                    List.length model.questionsPerRound

                newQuestionsPerRound =
                    if newRounds > currentLength then
                        model.questionsPerRound
                            |> List.append (List.repeat (newRounds - currentLength) defaultQuestionsPerRound)

                    else
                        List.take newRounds model.questionsPerRound
            in
            ( model
                |> Page.lenses.numberOfRounds.set newRounds
                |> Page.lenses.questionsPerRound.set newQuestionsPerRound
            , Cmd.none
            , Nothing
            )

        Page.SetQuestionsForRound roundIndex input ->
            let
                newValue =
                    String.toInt input
                        |> Maybe.withDefault defaultQuestionsPerRound
                        |> max 1

                newList =
                    model.questionsPerRound
                        |> List.indexedMap
                            (\i v ->
                                if i == roundIndex then
                                    newValue

                                else
                                    v
                            )
            in
            ( Page.lenses.questionsPerRound.set newList model
            , Cmd.none
            , Nothing
            )

        Page.SetNumberOfTeams input ->
            let
                newTeams =
                    String.toInt input
                        |> Maybe.withDefault model.numberOfTeams
                        |> max 1
            in
            ( Page.lenses.numberOfTeams.set newTeams model
            , Cmd.none
            , Nothing
            )

        Page.Submit ->
            case Date.fromIsoString model.date of
                Ok date ->
                    ( model
                        |> Page.lenses.isSubmitting.set True
                        |> Page.lenses.error.set Nothing
                    , createQuiz model date
                    , Nothing
                    )

                Err _ ->
                    ( Page.lenses.error.set (Just "Invalid date format") model
                    , Cmd.none
                    , Nothing
                    )

        Page.GotCreateResponse result ->
            case result of
                Ok quizActive ->
                    ( model |> Page.lenses.isSubmitting.set False
                    , Cmd.none
                    , Just quizActive.summary.quizId
                    )

                Err error ->
                    ( model
                        |> Page.lenses.isSubmitting.set False
                        |> Page.lenses.error.set (Just (errorToString error))
                    , Cmd.none
                    , Nothing
                    )


createQuiz : Page.Model -> Date.Date -> Cmd Page.Msg
createQuiz model date =
    Api.Api.backoffice
        { toMsg = Page.GotCreateResponse
        , body =
            QuizMetaData
                (QuizIdentifier date model.name model.place)
                (QuizSettings model.numberOfTeams model.questionsPerRound)
        }


errorToString : OpenApi.Common.Error () String -> String
errorToString error =
    case error of
        OpenApi.Common.BadUrl _ ->
            "Invalid URL"

        OpenApi.Common.Timeout ->
            "Request timed out"

        OpenApi.Common.NetworkError ->
            "Network error"

        OpenApi.Common.KnownBadStatus _ _ ->
            "Invalid quiz data"

        OpenApi.Common.UnknownBadStatus _ body ->
            String.concat [ "Error: ", body ]

        OpenApi.Common.BadErrorBody _ body ->
            String.concat [ "Error: ", body ]

        OpenApi.Common.BadBody _ body ->
            String.concat [ "Error: ", body ]
