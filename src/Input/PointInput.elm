module Input.PointInput exposing (..)

import Common.Authentication exposing (Authentication)
import Common.ConnectionUtil exposing (addFeedbackLabel, encodeBody, errorToString)
import Common.Constants exposing (mkPath, quizIdParam, quizRatingsParam, sheetPDFPrefix, updateApi)
import Common.Copy exposing (updateQuizSettingsNumberOfTeams)
import Common.QuizRatings as QuizRatings
import Common.RoundRating as RoundRating
import Common.Types exposing (DbQuizId, Header, QuizInfo, QuizRatings, QuizSettings, RoundNumber, RoundRating, TeamNumber, UserName, jsonEncDbQuizId, jsonEncQuizRatings)
import Common.Util exposing (adjustToSize)
import Html exposing (Html, a, button, div, input, label, text)
import Html.Attributes exposing (class, disabled, for, href, id, step, target, type_, value)
import Html.Events exposing (onClick, onInput)
import Http
import Input.Model exposing (ErrorOr)
import Input.RequestUtils exposing (SessionKey, encodeWithSignature)
import Parser exposing (float, int, run)


type alias Model =
    { quizInfo : QuizInfo
    , quizSettings : QuizSettings
    , quizRatings : QuizRatings
    , authentication : Authentication
    , feedback : String
    }


updateQuizInfo : Model -> QuizInfo -> Model
updateQuizInfo model quizInfo =
    { model | quizInfo = quizInfo }


updateQuizSettings : Model -> QuizSettings -> Model
updateQuizSettings model quizSettings =
    { model | quizSettings = quizSettings }


updateQuizRatings : Model -> QuizRatings -> Model
updateQuizRatings model quizRatings =
    { model | quizRatings = quizRatings }


updateFeedback : Model -> String -> Model
updateFeedback model feedback =
    { model | feedback = feedback }


type Msg
    = SetTeamsInQuiz String
    | AddRound
    | EditSettings
    | Back
    | AcknowledgeLock
    | UpdateQuizRatings
    | UpdatedQuizRatings (ErrorOr ())
    | SetTeamName TeamNumber String
    | SetMaxPoints RoundNumber String
    | UpdatePoints RoundNumber TeamNumber String


view : Model -> Html Msg
view md =
    let
        quizName =
            md.quizInfo.quizIdentifier.name

        numberOfTeams =
            md.quizSettings.numberOfTeams

        quizRatings =
            md.quizRatings
    in
    div [ id "singleQuiz" ]
        ([ div [ id "editingLabel" ]
            [ label [ for "editingQuiz" ]
                [ text (String.join " " [ "Editing", quizName ]) ]
            ]
         , div [ id "teamsInQuiz" ]
            [ label [ for "teamInQuizLabel" ] [ text "Teams in the quiz" ]
            , input
                [ value (String.fromInt numberOfTeams)
                , type_ "number"
                , min "1"
                , step "1"
                , max (String.fromInt (QuizRatings.maxNumberOfTeams quizRatings))
                , onInput SetTeamsInQuiz
                ]
                []
            ]
         , div [ id "teamNames" ]
            (label [ for "teamNamesLabel" ] [ text "Team names" ]
                :: mkTeamNameInput (List.take numberOfTeams quizRatings.header)
            )
         ]
            ++ List.map (\( rn, rr ) -> mkRoundForm rn numberOfTeams rr) quizRatings.ratings
            ++ [ button [ class "button", onClick AddRound ] [ text "Add round" ]
               , button [ class "button", onClick EditSettings ] [ text "Edit settings" ]
               , button [ class "backButton", onClick Back ] [ text "Back" ]
               , button [ class "lockButton", onClick AcknowledgeLock ] [ text "Lock" ]
               , button
                    [ class "button"
                    , onClick UpdateQuizRatings
                    ]
                    [ text "Update" ]
               , mkLinkToSheet "answerSheet" "Get quiz sheet" md.quizInfo.fullSheetPath
               , mkLinkToSheet "qrSheet" "Get QR codes only" md.quizInfo.qrOnlyPath

               -- todo: Adjust this path using a proper REST request
               , mkLinkToSheet "mainGraphPage" "View main graph page" ""
               , addFeedbackLabel md.feedback
               ]
        )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetTeamsInQuiz text ->
            let
                tu =
                    processTeamUpdate text model

                newQuizRatings =
                    QuizRatings.adjustTo tu.teams model.quizRatings
            in
            ( { model
                | quizSettings = updateQuizSettingsNumberOfTeams model.quizSettings tu.teams
                , quizRatings = newQuizRatings
                , feedback = tu.response
              }
            , Cmd.none
            )

        UpdatePoints rn tn ps ->
            let
                ( np, response ) =
                    case run float ps of
                        Ok p ->
                            let
                                maxPs =
                                    (QuizRatings.getRound rn model.quizRatings).reachableInRound
                            in
                            if p <= maxPs then
                                ( p, "" )

                            else
                                ( maxPs
                                , String.join " "
                                    [ "The maximum number of points"
                                    , "in this round is"
                                    , String.fromFloat maxPs
                                    ]
                                )

                        Err _ ->
                            ( 0
                            , String.join " "
                                [ "Invalid decimal point number"
                                , "at round ="
                                , String.fromInt rn
                                , "and team ="
                                , String.concat
                                    [ String.fromInt tn
                                    , "."
                                    ]
                                , "Substituting 0."
                                ]
                            )

                newQuiz =
                    QuizRatings.update rn tn np model.quizRatings
            in
            ( { model
                | quizRatings = newQuiz
                , feedback = response
              }
            , Cmd.none
            )

        AddRound ->
            let
                newModel =
                    QuizRatings.addRound (RoundRating.emptyOfSize model.quizSettings.numberOfTeams) model.quizRatings
                        |> updateQuizRatings model
            in
            ( newModel, Cmd.none )

        SetMaxPoints rd ps ->
            let
                newModel =
                    case run float ps of
                        Ok p ->
                            QuizRatings.updateMax rd p model.quizRatings
                                |> updateQuizRatings model

                        Err _ ->
                            { model | feedback = "Not a decimal point number." }
            in
            ( newModel, Cmd.none )

        UpdateQuizRatings ->
            ( model, postUpdate model.authentication model.quizInfo.quizId model.quizRatings )

        SetTeamName tn teamName ->
            let
                newQuizRatings =
                    QuizRatings.updateTeamName tn teamName model.quizRatings
            in
            ( { model
                | quizRatings = newQuizRatings
                , feedback = ""
              }
            , Cmd.none
            )

        UpdatedQuizRatings response ->
            let
                feedback =
                    case response of
                        Ok _ ->
                            "Update successful"

                        Err error ->
                            errorToString error
            in
            ( updateFeedback model feedback, Cmd.none )

        AcknowledgeLock ->
            ( model, Cmd.none )

        EditSettings ->
            ( model, Cmd.none )

        Back ->
            ( model, Cmd.none )


processTeamUpdate : String -> Model -> { teams : Int, response : String }
processTeamUpdate text model =
    let
        ( ts, r ) =
            case run int text of
                Ok n ->
                    let
                        maxTeams =
                            QuizRatings.maxNumberOfTeams model.quizRatings
                    in
                    if n <= maxTeams then
                        ( n, "" )

                    else
                        ( maxTeams
                        , String.join " "
                            [ "Quiz supports only"
                            , String.fromInt maxTeams
                            , "teams. Please edit the quiz settings to increase the number of teams."
                            ]
                        )

                Err _ ->
                    ( 0, "Invalid team number. Substituting 0." )
    in
    { teams = ts, response = r }


mkTeamNameInput : Header -> List (Html Msg)
mkTeamNameInput h =
    h
        |> List.sortBy .teamInfoNumber
        |> List.map (\ti -> mkSingleTeamNameInput ti.teamInfoNumber ti.teamInfoName)


mkSingleTeamNameInput : TeamNumber -> String -> Html Msg
mkSingleTeamNameInput tn name =
    div [ class "teamNameInputArea" ]
        [ label [ for "teamName" ] [ text (mkTeamNumber tn "Team") ]
        , input
            [ value name
            , onInput (SetTeamName tn)
            ]
            []
        ]


mkTeamNumber : TeamNumber -> String -> String
mkTeamNumber i wordForTeam =
    String.join " " [ wordForTeam, String.fromInt i ]


mkRoundForm : RoundNumber -> Int -> RoundRating -> Html Msg
mkRoundForm rn gs rr =
    div [ id "roundPoints" ]
        (label [ class "roundNumber" ]
            [ text (String.join " " [ "Round", String.fromInt rn ]) ]
            :: div [ id "maxPointsArea" ]
                [ label [ class "maxPoints" ] [ text "Obtainable" ]
                , input
                    (value (String.fromFloat rr.reachableInRound)
                        :: onInput (SetMaxPoints rn)
                        :: pointInputAttributes
                    )
                    []
                ]
            :: List.map
                (\tr ->
                    div [ class "teamPointsArea" ]
                        [ div [ class "label" ]
                            [ label [ class "pointsPerTeamLabel" ]
                                [ text
                                    (String.join " "
                                        [ "Team"
                                        , String.fromInt tr.teamNumber
                                        ]
                                    )
                                ]
                            ]
                        , div [ class "input" ]
                            [ input
                                (value (String.fromFloat tr.rating)
                                    :: onInput (UpdatePoints rn tr.teamNumber)
                                    :: max (String.fromFloat rr.reachableInRound)
                                    :: pointInputAttributes
                                )
                                []
                            ]
                        ]
                )
                (adjustToSize gs rr.points)
        )


mkLinkToSheet : String -> String -> String -> Html Msg
mkLinkToSheet divId linkText file =
    div [ id divId ]
        [ a
            [ class "link"
            , href
                (mkPath
                    [ sheetPDFPrefix
                    , file
                    ]
                )
            , target "_blank"
            ]
            [ text linkText ]
        ]


pointInputAttributes : List (Html.Attribute Msg)
pointInputAttributes =
    [ class "labeledInput", type_ "number", min "0", step "0.5" ]


postUpdate : Authentication -> DbQuizId -> QuizRatings -> Cmd Msg
postUpdate authentication qid quizRatings =
    let
        params =
            encodeWithSignature authentication
                [ ( quizIdParam, jsonEncDbQuizId qid )
                , ( quizRatingsParam, jsonEncQuizRatings quizRatings )
                ]
    in
    Http.post
        { url = updateApi
        , body = encodeBody params
        , expect = Http.expectWhatever UpdatedQuizRatings
        }
