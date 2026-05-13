module Pages.BackOffice.QuizEdit.View exposing (view)

import Api.Types exposing (Quiz, Round, Team)
import Dict exposing (Dict)
import Html exposing (Html, button, div, footer, h1, h2, header, input, label, li, ol, p, section, small, span, text, ul)
import Html.Attributes as Attr exposing (class, disabled, placeholder, step, type_, value)
import Html.Events exposing (onClick, onInput, preventDefaultOn)
import Json.Decode as Decode
import List.Extra
import Maybe.Extra
import Pages.BackOffice.QuizEdit.Page as Page
import Pages.BackOffice.Shared as Shared
import Set exposing (Set)
import Util.Team


view : Page.Model -> Html Page.Msg
view model =
    section [ class "quiz-edit" ]
        [ viewHeader model
        , viewContent model
        ]


viewHeader : Page.Model -> Html Page.Msg
viewHeader model =
    let
        quizName =
            model.quiz
                |> Maybe.Extra.unwrap "Loading..." (\q -> String.concat [ q.summary.identifier.name, " — Point Entry" ])
    in
    header [ class "quiz-edit-header" ]
        [ h1 [] [ text quizName ]
        , Shared.viewQuizNav model.quizId Shared.PointEntry
        ]


viewContent : Page.Model -> Html Page.Msg
viewContent model =
    if model.isLoading then
        p [ class "loading" ] [ text "Loading quiz..." ]

    else
        case model.quiz of
            Just quiz ->
                section [ class "quiz-edit-content" ]
                    [ viewError model.error
                    , viewRounds model quiz
                    , viewAddRound model
                    ]

            Nothing ->
                p [ class "error" ] [ text "Failed to load quiz" ]


viewError : Maybe String -> Html msg
viewError =
    Maybe.Extra.unwrap (text "") (\error -> p [ class "form-error" ] [ text error ])


viewRounds : Page.Model -> Quiz -> Html Page.Msg
viewRounds model quiz =
    ol [ class "rounds-accordion" ]
        (List.map (viewRound model quiz) quiz.rounds)


viewRound : Page.Model -> Quiz -> Round -> Html Page.Msg
viewRound model quiz round =
    let
        isExpanded =
            model.expandedRound == Just round.number

        isComplete =
            Set.member round.number model.completedRounds

        isEditing =
            model.editingRound == Just round.number

        roundState =
            if isEditing then
                Page.Editing

            else if isComplete then
                Page.Complete

            else
                Page.Draft

        roundClass =
            String.join " "
                [ "round"
                , if isExpanded then
                    "expanded"

                  else
                    "collapsed"
                , roundStateClass roundState
                ]
    in
    li [ class roundClass ]
        [ viewRoundHeader model round isExpanded isComplete isEditing
        , if isExpanded then
            viewRoundBody model quiz round roundState

          else
            text ""
        ]


roundStateClass : Page.RoundState -> String
roundStateClass state =
    case state of
        Page.Draft ->
            "draft"

        Page.Complete ->
            "complete"

        Page.Editing ->
            "editing"


viewRoundHeader : Page.Model -> Round -> Bool -> Bool -> Bool -> Html Page.Msg
viewRoundHeader model round isExpanded isComplete isEditing =
    let
        headerAction =
            if isExpanded then
                Page.CollapseRound

            else
                Page.ExpandRound round.number

        statusIcon =
            if isComplete && not isEditing then
                span [ class "status-icon complete" ] [ text "✓" ]

            else if isEditing then
                span [ class "status-icon editing" ] [ text "✎" ]

            else
                span [ class "status-icon draft" ] [ text "○" ]

        summary =
            getRoundSummary model round

        roundLabel =
            String.concat [ "Round ", String.fromInt round.number ]

        maxPointsLabel =
            String.concat [ String.fromFloat round.displayMaxPoints, " points possible" ]

        expandIconText =
            if isExpanded then
                "▼"

            else
                "▶"
    in
    header [ class "round-header", onClick headerAction ]
        [ statusIcon
        , h2 [] [ text roundLabel ]
        , small [ class "max-points" ] [ text maxPointsLabel ]
        , if not isExpanded && summary /= "" then
            small [ class "round-summary" ] [ text summary ]

          else
            text ""
        , span [ class "expand-icon" ] [ text expandIconText ]
        ]


getRoundSummary : Page.Model -> Round -> String
getRoundSummary model round =
    case Dict.get round.number model.roundInputs of
        Just roundInput ->
            let
                filledCount =
                    roundInput.scores
                        |> Dict.values
                        |> List.Extra.count (String.isEmpty >> not)

                totalCount =
                    Dict.size roundInput.scores
            in
            String.concat [ String.fromInt filledCount, "/", String.fromInt totalCount, " teams" ]

        Nothing ->
            ""


viewRoundBody : Page.Model -> Quiz -> Round -> Page.RoundState -> Html Page.Msg
viewRoundBody model quiz round state =
    let
        isEditable =
            state == Page.Draft || state == Page.Editing

        roundInput =
            Dict.get round.number model.roundInputs
                |> Maybe.withDefault { scores = Dict.empty }
    in
    section [ class "round-body" ]
        [ ul [ class "teams-grid" ]
            (List.map (viewTeamScore model round roundInput isEditable) quiz.scoreBoard.teams)
        , viewRoundActions model round state
        ]


viewTeamScore : Page.Model -> Round -> Page.RoundInput -> Bool -> Team -> Html Page.Msg
viewTeamScore model round roundInput isEditable team =
    let
        scoreValue =
            Dict.get team.number roundInput.scores
                |> Maybe.withDefault ""

        teamLabel =
            Util.Team.teamName team

        teamClass =
            if team.active then
                "team-score"

            else
                "team-score inactive"

        isDisabled =
            not isEditable || model.isSubmitting || model.isLocked
    in
    li [ class teamClass ]
        [ label [] [ text teamLabel ]
        , div [ class "score-input-group" ]
            [ button
                [ class "stepper decrement"
                , onClick (Page.IncrementScore round.number team.number -0.5)
                , disabled isDisabled
                ]
                [ text "−½" ]
            , input
                [ type_ "number"
                , class "score-input"
                , value scoreValue
                , onInput (Page.SetScore round.number team.number)
                , placeholder "0"
                , step "0.5"
                , Attr.min "0"
                , disabled isDisabled
                , onScoreKeydown round.number team.number
                ]
                []
            , button
                [ class "stepper increment-half"
                , onClick (Page.IncrementScore round.number team.number 0.5)
                , disabled isDisabled
                ]
                [ text "+½" ]
            , button
                [ class "stepper increment-full"
                , onClick (Page.IncrementScore round.number team.number 1)
                , disabled isDisabled
                ]
                [ text "+1" ]
            ]
        ]


viewRoundActions : Page.Model -> Round -> Page.RoundState -> Html Page.Msg
viewRoundActions model round state =
    let
        isDisabled =
            model.isSubmitting || model.isLocked

        submitLabel =
            case state of
                Page.Draft ->
                    if model.isSubmitting then
                        "Submitting..."

                    else
                        "Submit Scores"

                Page.Editing ->
                    if model.isSubmitting then
                        "Saving..."

                    else
                        "Save Changes"

                Page.Complete ->
                    ""
    in
    footer [ class "round-actions" ]
        (case state of
            Page.Draft ->
                [ button
                    [ class "button primary"
                    , onClick (Page.SubmitRound round.number)
                    , disabled isDisabled
                    ]
                    [ text submitLabel ]
                , button
                    [ class "button secondary"
                    , onClick (Page.MarkRoundComplete round.number)
                    , disabled isDisabled
                    ]
                    [ text "Publish" ]
                ]

            Page.Complete ->
                [ button
                    [ class "button secondary"
                    , onClick (Page.EditCompletedRound round.number)
                    , disabled model.isLocked
                    ]
                    [ text "Edit" ]
                ]

            Page.Editing ->
                [ button
                    [ class "button primary"
                    , onClick (Page.SubmitRound round.number)
                    , disabled isDisabled
                    ]
                    [ text submitLabel ]
                , button
                    [ class "button secondary"
                    , onClick Page.CancelEdit
                    , disabled isDisabled
                    ]
                    [ text "Cancel" ]
                ]
        )


viewAddRound : Page.Model -> Html Page.Msg
viewAddRound model =
    footer [ class "add-round" ]
        [ button
            [ class "button secondary"
            , onClick Page.AddRound
            , disabled model.isLocked
            ]
            [ text "+ Add Round" ]
        ]


onScoreKeydown : Int -> Int -> Html.Attribute Page.Msg
onScoreKeydown roundNumber teamNumber =
    preventDefaultOn "keydown" (scoreKeyDecoder roundNumber teamNumber)


scoreKeyDecoder : Int -> Int -> Decode.Decoder ( Page.Msg, Bool )
scoreKeyDecoder roundNumber teamNumber =
    Decode.map2 Tuple.pair
        (Decode.field "key" Decode.string)
        (Decode.field "shiftKey" Decode.bool)
        |> Decode.andThen (decodeArrowKey roundNumber teamNumber)


decodeArrowKey : Int -> Int -> ( String, Bool ) -> Decode.Decoder ( Page.Msg, Bool )
decodeArrowKey roundNumber teamNumber ( key, shiftKey ) =
    let
        delta =
            deltaForModifier shiftKey
    in
    case key of
        "ArrowUp" ->
            Decode.succeed ( Page.IncrementScore roundNumber teamNumber delta, True )

        "ArrowDown" ->
            Decode.succeed ( Page.IncrementScore roundNumber teamNumber -delta, True )

        _ ->
            Decode.fail "not an arrow key"


deltaForModifier : Bool -> Float
deltaForModifier shiftKey =
    if shiftKey then
        0.5

    else
        1
