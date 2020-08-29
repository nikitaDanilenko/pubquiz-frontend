module Input.PointInput exposing (..)

import Basics.Extra exposing (flip, uncurry)
import Common.Authentication exposing (Authentication, encodeWithSignature)
import Common.Constants exposing (getLabelsApi, getQuizRatingsApi, mkPath, quizIdParam, quizRatingsParam, serverLocation, serverQuizzesFolder, updateQuizRatingsApi)
import Common.Copy exposing (updateHeaderTeamInfo, updateTeamInfoActivity)
import Common.FromInput exposing (FromInput)
import Common.HttpUtil as HttpUtil
import Common.NumberInputs.RatingsInput as RatingsInput exposing (RatingsInput)
import Common.NumberInputs.RoundRatingInput as RoundRatingInput
import Common.NumberInputs.TeamRatingInput exposing (TeamRatingInput)
import Common.QuizRatings as QuizRatings
import Common.Ranking exposing (NamedTeamRating, ratingsToRankings)
import Common.Types exposing (Activity, DbQuizId, Header, Labels, QuizInfo, QuizRatings, QuizSettings, RoundNumber, RoundRating, TeamInfo, TeamNumber, UserName, jsonDecLabels, jsonDecQuizRatings, jsonEncDbQuizId, jsonEncQuizRatings)
import Common.Util as Util exposing (ErrorOr, getMsg, special)
import Common.WireUtil exposing (addFeedbackLabel, encodeBody, errorToString, loadingSymbol, mkPlacementTables)
import Html exposing (Html, a, button, div, input, label, text)
import Html.Attributes exposing (checked, class, disabled, for, href, id, max, tabindex, target, type_, value)
import Html.Events exposing (on, onClick, onInput)
import Http
import Input.QuizValues as QuizValues exposing (defaultLabels)
import Keyboard.Event exposing (KeyboardEvent)
import Keyboard.Key exposing (Key(..))
import List.Extra
import Output.OutputUtil exposing (fromServerUrl)


type alias Model =
    { quizInfo : QuizInfo
    , labels : Labels
    , quizRatings : QuizRatings
    , ratingsInput : RatingsInput
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


updateRatingsInput : Model -> RatingsInput -> Model
updateRatingsInput model ratingsInput =
    { model | ratingsInput = ratingsInput, quizRatings = RatingsInput.toRatings ratingsInput |> QuizRatings.updateRatings model.quizRatings }


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


type Direction
    = More
    | Less


step : Float
step =
    0.5


updateByDirection : Direction -> Float -> Float
updateByDirection direction x =
    case direction of
        More ->
            x + step

        Less ->
            x - step


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
    | SwapTeamActivity TeamNumber
    | GotLabels (ErrorOr Labels)
    | GotQuizRatings (ErrorOr QuizRatings)
    | ChangeMaxPoints RoundNumber Direction
    | ChangePoints RoundNumber TeamNumber Direction


mkKeyEventMsg : (Direction -> Msg) -> KeyboardEvent -> Maybe Msg
mkKeyEventMsg toMsg keyboardEvent =
    case keyboardEvent.keyCode of
        Up ->
            More |> toMsg |> Just

        Down ->
            Less |> toMsg |> Just

        _ ->
            Nothing


onKeyPress : (Direction -> Msg) -> Html.Attribute Msg
onKeyPress toMsg =
    toMsg |> mkKeyEventMsg |> Keyboard.Event.considerKeyboardEvent |> on "keydown"


init : Authentication -> QuizInfo -> ( Model, Cmd Msg )
init authentication quizInfo =
    ( { quizInfo = quizInfo
      , labels = defaultLabels
      , quizRatings = QuizRatings.default
      , ratingsInput = RatingsInput.fromRatings QuizRatings.defaultRatings
      , authentication = authentication
      , feedback = ""
      , status = initialStatus
      }
    , Cmd.batch [ getLabels quizInfo.quizId, getQuizRatings quizInfo.quizId ]
    )



-- todo: This is precisely the same function as in Ranking, but it operates on another type. Unify these?


mkActiveAndNamed : Header -> List TeamRatingInput -> List { teamRating : TeamRatingInput, teamName : String }
mkActiveAndNamed header tris =
    Util.intersectWith Tuple.pair .teamNumber identity .teamInfoNumber (\ti -> ( ti.teamInfoName, ti.teamInfoActivity )) tris header
        |> List.filter (Tuple.second >> Tuple.second >> QuizValues.isActive)
        |> List.map (\( teamRating, ( teamName, _ ) ) -> { teamRating = teamRating, teamName = teamName })


view : Model -> Html Msg
view model =
    let
        rankings =
            ratingsToRankings model.quizRatings

        namedRatings =
            List.map
                (Tuple.mapSecond
                    (\roundRating ->
                        { reachableInRound = roundRating.reachableInRound
                        , points = mkActiveAndNamed rankings.sortedHeader roundRating.points
                        }
                    )
                )
                model.ratingsInput
    in
    if not (hasFinishedLoading model.status) then
        div [] [ loadingSymbol ]

    else
        div [ id "singleQuiz" ]
            ([ div [ id "editingLabel" ]
                [ label [ for "editingQuiz" ]
                    [ text (String.join " " [ "Editing", model.quizInfo.quizIdentifier.name ]) ]
                ]
             , div [ id "teamNames" ]
                (label [ for "teamNamesLabel" ] [ text "Team names" ]
                    :: mkTeamNameInput rankings.sortedHeader
                )
             ]
                ++ List.map (uncurry mkRoundForm) namedRatings
                ++ [ button [ class "button", onClick AddRound ] [ text "Add round" ]
                   , button [ class "button", onClick EditSettings ] [ text "Edit settings" ]
                   , button [ class "backButton", onClick Back ] [ text "Back" ]
                   , button [ class "lockButton", onClick AcknowledgeLock ] [ text "Lock" ]
                   , button
                        [ class "button"
                        , onClick UpdateQuizRatings
                        , disabled (List.any (.teamInfoName >> String.isEmpty) model.quizRatings.header)
                        ]
                        [ text "Update" ]
                   , mkLinkToSheet "answerSheet" "Get quiz sheet" model.quizInfo.fullSheetPath
                   , mkLinkToSheet "qrSheet" "Get QR codes only" model.quizInfo.qrOnlyPath
                   , mkLinkWith "mainGraphPage"
                        "View main graph page"
                        (fromServerUrl [ serverQuizzesFolder ] [ quizIdParam, String.fromInt model.quizInfo.quizId ])
                   , addFeedbackLabel model.feedback
                   , div [ id "pointInputRankings" ] (mkPlacementTables rankings model.labels)
                   ]
            )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdatePoints rn tn ps ->
            ( updateTeamPointsInRound rn tn ps model, Cmd.none )

        AddRound ->
            let
                newModel =
                    RatingsInput.addRound (RoundRatingInput.emptyForHeader model.quizRatings.header) model.ratingsInput
                        |> updateRatingsInput model
            in
            ( newModel, Cmd.none )

        SetMaxPoints rd ps ->
            ( updateMaxPointsInRound rd ps model, Cmd.none )

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
                            RatingsInput.fromRatings quizRatings.ratings
                                |> updateRatingsInput model
                                |> (\md -> md.quizRatings |> flip QuizRatings.updateHeader quizRatings.header |> updateQuizRatings md)
                                |> (\md -> updateQuizRatingsLoaded md.status True |> updateStatus md)
                    in
                    ( newModel, Cmd.none )

                Err error ->
                    ( updateFeedback model (errorToString error), Cmd.none )

        SwapTeamActivity teamNumber ->
            -- This list should always contain exactly one element.
            let
                teamInfoList =
                    List.filter (\ti -> ti.teamInfoNumber == teamNumber) model.quizRatings.header

                newModel =
                    List.foldr
                        (\ti md ->
                            QuizValues.swapActivity ti.teamInfoActivity
                                |> updateTeamInfoActivity ti
                                |> updateHeaderTeamInfo md.quizRatings.header
                                |> QuizRatings.updateHeader md.quizRatings
                                |> updateQuizRatings md
                        )
                        model
                        teamInfoList
            in
            ( newModel, Cmd.none )

        ChangeMaxPoints roundNumber direction ->
            let
                currentPoints =
                    List.Extra.find (\( rn, _ ) -> rn == roundNumber) model.quizRatings.ratings
                        |> Util.foldMaybe 0 (Tuple.second >> .reachableInRound)

                newPoints =
                    updateByDirection direction currentPoints

                newModel =
                    updateMaxPointsInRound roundNumber (String.fromFloat newPoints) model
            in
            ( newModel, Cmd.none )

        ChangePoints roundNumber teamNumber direction ->
            let
                currentPoints =
                    List.Extra.find (\( rn, _ ) -> rn == roundNumber) model.quizRatings.ratings
                        |> Maybe.andThen (\( _, rr ) -> List.Extra.find (\tr -> tr.teamNumber == teamNumber) rr.points)
                        |> Util.foldMaybe 0 .rating

                newPoints =
                    updateByDirection direction currentPoints

                newModel =
                    updateTeamPointsInRound roundNumber teamNumber (String.fromFloat newPoints) model
            in
            ( newModel, Cmd.none )


updateMaxPointsInRound : RoundNumber -> String -> Model -> Model
updateMaxPointsInRound roundNumber newPoints model =
    let
        newRatingsInput =
            model.ratingsInput |> RatingsInput.updateMax roundNumber newPoints
    in
    RatingsInput.toRatings newRatingsInput
        |> QuizRatings.updateRatings model.quizRatings
        |> updateQuizRatings model
        |> flip updateRatingsInput newRatingsInput


updateTeamPointsInRound : RoundNumber -> TeamNumber -> String -> Model -> Model
updateTeamPointsInRound rn tn ps model =
    model.ratingsInput
        |> RatingsInput.updatePoints rn tn ps
        |> updateRatingsInput model


mkTeamNameInput : Header -> List (Html Msg)
mkTeamNameInput =
    List.sortBy .teamInfoNumber >> List.map mkSingleTeamNameInput


mkSingleTeamNameInput : TeamInfo -> Html Msg
mkSingleTeamNameInput teamInfo =
    div [ class "teamNameInputArea" ]
        [ label [ for "teamName" ] [ text (mkTeamNumber teamInfo.teamInfoNumber "Team") ]
        , input
            [ value teamInfo.teamInfoName
            , onInput (SetTeamName teamInfo.teamInfoNumber)
            ]
            []
        , label [ for "teamActivity" ] [ text "Active?" ]
        , input
            [ type_ "checkbox"
            , checked (QuizValues.isActive teamInfo.teamInfoActivity)
            , onClick (SwapTeamActivity teamInfo.teamInfoNumber)
            ]
            []
        ]


mkTeamNumber : TeamNumber -> String -> String
mkTeamNumber i wordForTeam =
    String.join " " [ wordForTeam, String.fromInt i ]


type alias NamedRoundRating =
    { reachableInRound : Float
    , points : List NamedTeamRating
    }


mkDirectionalButton : Direction -> (Direction -> Msg) -> Html Msg
mkDirectionalButton direction toMsg =
    let
        symbol =
            case direction of
                More ->
                    special 9650

                Less ->
                    special 9660
    in
    button
        [ class "directionalButton"
        , tabindex -1
        , onClick (toMsg direction)
        ]
        [ label [ class "directionalLabel" ] [ text symbol ] ]


mkDirectionalButtons : (Direction -> Msg) -> Html Msg
mkDirectionalButtons toMsg =
    div [ class "directionalButtonGroup" ]
        [ mkDirectionalButton More toMsg, mkDirectionalButton Less toMsg ]


mkRoundForm : RoundNumber -> { reachableInRound : FromInput Float, points : List { teamRating : TeamRatingInput, teamName : String } } -> Html Msg
mkRoundForm roundNumber sortedNamedRoundRating =
    div [ id "roundPoints" ]
        (label [ class "roundNumber" ]
            [ text (String.join " " [ "Round", String.fromInt roundNumber ]) ]
            :: div [ id "maxPointsArea" ]
                [ label [ class "maxPoints" ] [ text "Obtainable" ]
                , input
                    (value sortedNamedRoundRating.reachableInRound.text
                        :: onInput (SetMaxPoints roundNumber)
                        :: onKeyPress (ChangeMaxPoints roundNumber)
                        :: pointInputAttributes
                    )
                    []
                , mkDirectionalButtons (ChangeMaxPoints roundNumber)
                ]
            :: List.map
                (\namedRoundRating ->
                    div [ class "teamPointsArea" ]
                        [ div [ class "label" ]
                            [ label [ for "pointsPerTeamLabel" ]
                                [ text namedRoundRating.teamName ]
                            ]
                        , div [ class "input" ]
                            [ input
                                (value namedRoundRating.teamRating.rating.text
                                    :: onInput (UpdatePoints roundNumber namedRoundRating.teamRating.teamNumber)
                                    :: onKeyPress (ChangePoints roundNumber namedRoundRating.teamRating.teamNumber)
                                    :: max sortedNamedRoundRating.reachableInRound.text
                                    :: pointInputAttributes
                                )
                                []
                            , mkDirectionalButtons (ChangePoints roundNumber namedRoundRating.teamRating.teamNumber)
                            ]
                        ]
                )
                sortedNamedRoundRating.points
        )


mkLinkToSheet : String -> String -> String -> Html Msg
mkLinkToSheet divId linkText file =
    mkLinkWith divId linkText (mkPath [ serverLocation, file ])


mkLinkWith : String -> String -> String -> Html Msg
mkLinkWith divId linkText path =
    div [ id divId ]
        [ a
            [ class "link"
            , href path
            , target "_blank"
            ]
            [ text linkText ]
        ]


pointInputAttributes : List (Html.Attribute Msg)
pointInputAttributes =
    [ class "labeledInput" ]


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
        , expect = HttpUtil.expectWhatever UpdatedQuizRatings
        }


getLabels : DbQuizId -> Cmd Msg
getLabels =
    getMsg getLabelsApi GotLabels jsonDecLabels


getQuizRatings : DbQuizId -> Cmd Msg
getQuizRatings =
    getMsg getQuizRatingsApi GotQuizRatings jsonDecQuizRatings
