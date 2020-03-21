module Input.PointInput exposing (..)

import Basics.Extra exposing (flip)
import Common.Authentication exposing (Authentication, encodeWithSignature)
import Common.Constants
    exposing
        ( getLabelsApi
        , getQuizRatingsApi
        , mkPath
        , quizIdParam
        , quizRatingsParam
        , sheetPDFPrefix
        , updateQuizRatingsApi
        )
import Common.QuizRatings as QuizRatings
import Common.Ranking exposing (ratingsToRankings)
import Common.RoundRating as RoundRating
import Common.Types exposing (Activity, DbQuizId, Header, Labels, QuizInfo, QuizRatings, QuizSettings, RoundNumber, RoundRating, TeamInfo, TeamNumber, UserName, jsonDecLabels, jsonDecQuizRatings, jsonEncDbQuizId, jsonEncQuizRatings)
import Common.Util exposing (ErrorOr, adjustToSize, getMsg)
import Common.WireUtil exposing (addFeedbackLabel, encodeBody, errorToString, mkPlacementTables)
import Html exposing (Html, a, button, div, input, label, text)
import Html.Attributes exposing (class, for, href, id, max, min, step, target, type_, value)
import Html.Events exposing (onClick, onInput)
import Http
import Input.QuizValues as QuizValues exposing (defaultLabels)
import Parser exposing (float, run)


type alias Model =
    { quizInfo : QuizInfo
    , labels : Labels
    , quizRatings : QuizRatings
    , authentication : Authentication
    , feedback : String
    , status : Status
    }


updateLabels : Model -> Labels -> Model
updateLabels model labels =
    { model | labels = labels }


updateQuizRatings : Model -> QuizRatings -> Model
updateQuizRatings model quizRatings =
    { model | quizRatings = quizRatings }


updateFeedback : Model -> String -> Model
updateFeedback model feedback =
    { model | feedback = feedback }


updateStatus : Model -> Status -> Model
updateStatus model status =
    { model | status = status }


type alias Status =
    { labelsLoaded : Bool
    , quizRatingsLoaded : Bool
    }


initialStatus : Status
initialStatus =
    { labelsLoaded = False
    , quizRatingsLoaded = False
    }


updateLabelsLoaded : Status -> Bool -> Status
updateLabelsLoaded status b =
    { status | labelsLoaded = b }


updateQuizRatingsLoaded : Status -> Bool -> Status
updateQuizRatingsLoaded status b =
    { status | quizRatingsLoaded = b }


hasFinishedLoading : Status -> Bool
hasFinishedLoading status =
    List.all identity [ status.labelsLoaded, status.quizRatingsLoaded ]


type Msg
    = AddRound
    | EditSettings
    | Back
    | AcknowledgeLock
    | UpdateQuizRatings
    | UpdatedQuizRatings (ErrorOr ())
    | SetTeamName TeamNumber String
    | SetMaxPoints RoundNumber String
    | UpdatePoints RoundNumber TeamNumber String
    | UpdateTeamActivity TeamNumber Activity
    | GotLabels (ErrorOr Labels)
    | GotQuizRatings (ErrorOr QuizRatings)


init : Authentication -> QuizInfo -> ( Model, Cmd Msg )
init authentication quizInfo =
    ( { quizInfo = quizInfo
      , labels = defaultLabels
      , quizRatings = QuizRatings.empty
      , authentication = authentication
      , feedback = ""
      , status = initialStatus
      }
    , Cmd.batch [ getLabels quizInfo.quizId, getQuizRatings quizInfo.quizId ]
    )


view : Model -> Html Msg
view model =
    let
        quizName =
            model.quizInfo.quizIdentifier.name

        numberOfActiveTeams =
            model.quizRatings.header
                |> activeTeams
                |> List.length

        quizRatings =
            model.quizRatings

        rankings =
            ratingsToRankings quizRatings
    in
    if not (hasFinishedLoading model.status) then
        div [] []

    else
        div [ id "singleQuiz" ]
            ([ div [ id "editingLabel" ]
                [ label [ for "editingQuiz" ]
                    [ text (String.join " " [ "Editing", quizName ]) ]
                ]

             -- todo: Adjust to set teams inactive.
             , div [ id "teamNames" ]
                (label [ for "teamNamesLabel" ] [ text "Team names" ]
                    :: mkTeamNameInput quizRatings.header
                )
             ]
                -- todo: Change round form so that the name of the team is displayed
                ++ List.map (\( rn, rr ) -> mkRoundForm rn numberOfActiveTeams rr) quizRatings.ratings
                ++ [ button [ class "button", onClick AddRound ] [ text "Add round" ]
                   , button [ class "button", onClick EditSettings ] [ text "Edit settings" ]
                   , button [ class "backButton", onClick Back ] [ text "Back" ]
                   , button [ class "lockButton", onClick AcknowledgeLock ] [ text "Lock" ]
                   , button
                        [ class "button"
                        , onClick UpdateQuizRatings
                        ]
                        [ text "Update" ]
                   , mkLinkToSheet "answerSheet" "Get quiz sheet" model.quizInfo.fullSheetPath
                   , mkLinkToSheet "qrSheet" "Get QR codes only" model.quizInfo.qrOnlyPath

                   -- todo: Adjust this path using a proper REST request
                   , mkLinkToSheet "mainGraphPage" "View main graph page" ""
                   , addFeedbackLabel model.feedback
                   , div [ id "pointInputRankings" ] (mkPlacementTables rankings model.labels)
                   ]
            )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdatePoints rn tn ps ->
            let
                newPoints =
                    computeNewPoints rn tn ps model.quizRatings

                newModel =
                    QuizRatings.update model.quizRatings rn tn newPoints.points
                        |> updateQuizRatings model
                        |> flip updateFeedback newPoints.feedback
            in
            ( newModel, Cmd.none )

        AddRound ->
            let
                newModel =
                    QuizRatings.addRound (RoundRating.emptyOfSize (List.length (activeTeams model.quizRatings.header))) model.quizRatings
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
                            { model | feedback = "Not a decimal point number. Keeping old value." }
            in
            ( newModel, Cmd.none )

        UpdateQuizRatings ->
            ( model, postUpdate model.authentication model.quizInfo.quizId model.quizRatings )

        SetTeamName tn teamName ->
            let
                newModel =
                    QuizRatings.updateTeamName tn teamName model.quizRatings
                        |> updateQuizRatings model
                        |> flip updateFeedback ""
            in
            ( newModel, Cmd.none )

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

        GotLabels labelsCandidate ->
            case labelsCandidate of
                Ok labels ->
                    let
                        newModel =
                            updateLabelsLoaded model.status True
                                |> updateStatus model
                                |> flip updateLabels labels
                    in
                    ( newModel, Cmd.none )

                Err error ->
                    ( updateFeedback model (errorToString error), Cmd.none )

        GotQuizRatings quizRatingsCandidate ->
            case quizRatingsCandidate of
                Ok quizRatings ->
                    let
                        newModel =
                            updateQuizRatings model quizRatings
                                |> (\md -> updateQuizRatingsLoaded md.status True |> updateStatus md)
                    in
                    ( newModel, Cmd.none )

                Err error ->
                    ( updateFeedback model (errorToString error), Cmd.none )

        UpdateTeamActivity teamNumber activity ->
          -- todo: implement this case
            ( model, Cmd.none )


computeNewPoints : RoundNumber -> TeamNumber -> String -> QuizRatings -> { points : Float, feedback : String }
computeNewPoints roundNumber teamNumber points quizRatings =
    let
        ( newPoints, feedback ) =
            case run float points of
                Ok p ->
                    let
                        maxPs =
                            (QuizRatings.getRound roundNumber quizRatings).reachableInRound
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
                        , String.fromInt roundNumber
                        , "and team ="
                        , String.concat
                            [ String.fromInt teamNumber
                            , "."
                            ]
                        , "Substituting 0."
                        ]
                    )
    in
    { points = newPoints, feedback = feedback }


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
                (adjustToSize gs (List.sortBy .teamNumber rr.points))
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


activeTeams : Header -> Header
activeTeams =
    List.filter (.teamInfoActivity >> QuizValues.isActive)


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
        { url = updateQuizRatingsApi
        , body = encodeBody params
        , expect = Http.expectWhatever UpdatedQuizRatings
        }


getLabels : DbQuizId -> Cmd Msg
getLabels =
    getMsg getLabelsApi GotLabels jsonDecLabels


getQuizRatings : DbQuizId -> Cmd Msg
getQuizRatings =
    getMsg getQuizRatingsApi GotQuizRatings jsonDecQuizRatings
