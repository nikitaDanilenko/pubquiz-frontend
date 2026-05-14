module Pages.BackOffice.QuizSettings.View exposing (view)

import Api.Types exposing (Quiz, Team)
import Dict exposing (Dict)
import Html exposing (Html, button, footer, h1, h2, header, input, label, li, p, section, strong, text, ul)
import Html.Attributes as Attr exposing (checked, class, disabled, for, id, placeholder, type_, value)
import Html.Events exposing (onCheck, onClick, onInput)
import Maybe.Extra
import Pages.BackOffice.QuizSettings.Page as Page
import Pages.BackOffice.Shared as Shared


view : Page.Model -> Html Page.Msg
view model =
    section [ class "quiz-settings" ]
        [ viewHeader model
        , viewContent model
        ]


viewHeader : Page.Model -> Html Page.Msg
viewHeader model =
    let
        title =
            model.quiz
                |> Maybe.Extra.unwrap "Loading..." (\q -> String.concat [ q.summary.identifier.name, " — Settings" ])
    in
    header [ class "quiz-settings-header" ]
        [ h1 [] [ text title ]
        , Shared.viewQuizNav model.quizId Shared.Settings
        ]


viewContent : Page.Model -> Html Page.Msg
viewContent model =
    if model.isLoading then
        p [ class "loading" ] [ text "Loading settings..." ]

    else
        case model.quiz of
            Just quiz ->
                section [ class "quiz-settings-content" ]
                    [ viewMessages model
                    , viewLockSection model
                    , viewIdentifierSection model
                    , viewRoundsSection model
                    , viewTeamsSection model quiz
                    ]

            Nothing ->
                p [ class "error" ] [ text "Failed to load quiz" ]


viewMessages : Page.Model -> Html msg
viewMessages model =
    let
        errorView =
            Maybe.Extra.unwrap (text "") (\e -> p [ class "form-error" ] [ text e ]) model.error

        successView =
            Maybe.Extra.unwrap (text "") (\m -> p [ class "form-success" ] [ text m ]) model.successMessage

        hasMessages =
            Maybe.Extra.isJust model.error || Maybe.Extra.isJust model.successMessage
    in
    if hasMessages then
        section [ class "messages" ]
            [ errorView
            , successView
            ]

    else
        text ""


viewIdentifierSection : Page.Model -> Html Page.Msg
viewIdentifierSection model =
    let
        isDisabled =
            model.isSaving || model.isLocked
    in
    section [ class "settings-section" ]
        [ h2 [] [ text "Quiz Details" ]
        , section [ class "form-fields" ]
            [ p [ class "form-field" ]
                [ label [ for "name" ] [ text "Name" ]
                , input [ type_ "text", id "name", value model.name, onInput Page.SetName, disabled isDisabled ] []
                ]
            , p [ class "form-field" ]
                [ label [ for "date" ] [ text "Date" ]
                , input [ type_ "date", id "date", value model.date, onInput Page.SetDate, disabled isDisabled ] []
                ]
            , p [ class "form-field" ]
                [ label [ for "place" ] [ text "Place" ]
                , input [ type_ "text", id "place", value model.place, onInput Page.SetPlace, disabled isDisabled ] []
                ]
            ]
        , footer [ class "section-actions" ]
            [ button
                [ class "button primary"
                , onClick Page.SaveIdentifier
                , disabled isDisabled
                ]
                [ text (saveButtonLabel model.isSaving "Save Details") ]
            ]
        ]


viewRoundsSection : Page.Model -> Html Page.Msg
viewRoundsSection model =
    let
        isDisabled =
            model.isSaving || model.isLocked

        buttonLabel =
            saveButtonLabel model.isSaving "Save Rounds"

        roundRows =
            model.questionsPerRound
                |> Dict.toList
                |> List.sortBy Tuple.first
                |> List.map (\( roundNumber, questionCount ) -> viewRoundRow model roundNumber questionCount)
    in
    section [ class "settings-section" ]
        [ h2 [] [ text "Questions per Round" ]
        , section [ class "round-inputs" ] roundRows
        , footer [ class "section-actions" ]
            [ button
                [ class "button secondary"
                , onClick Page.AddRound
                , disabled isDisabled
                ]
                [ text "Add Round" ]
            , button
                [ class "button primary"
                , onClick Page.SaveIdentifier
                , disabled isDisabled
                ]
                [ text buttonLabel ]
            ]
        ]


viewRoundRow : Page.Model -> Int -> Int -> Html Page.Msg
viewRoundRow model roundNumber questionCount =
    let
        rowId =
            String.concat [ "round-", String.fromInt roundNumber ]

        roundLabel =
            String.concat [ "Round ", String.fromInt roundNumber ]

        isDisabled =
            model.isSaving || model.isLocked
    in
    section [ class "round-input" ]
        [ label [ for rowId ] [ text roundLabel ]
        , input
            [ type_ "number"
            , id rowId
            , value (String.fromInt questionCount)
            , onInput (Page.SetQuestionsForRound roundNumber)
            , Attr.min "0"
            , disabled isDisabled
            ]
            []
        ]


viewTeamsSection : Page.Model -> Quiz -> Html Page.Msg
viewTeamsSection model quiz =
    section [ class "settings-section" ]
        [ h2 [] [ text "Teams" ]
        , ul [ class "teams-list" ]
            (List.map (viewTeamRow model) quiz.scoreBoard.teams)
        ]


viewTeamRow : Page.Model -> Team -> Html Page.Msg
viewTeamRow model team =
    let
        teamName =
            Dict.get team.number model.teamNames
                |> Maybe.withDefault team.name

        rowClass =
            if team.active then
                "team-row"

            else
                "team-row inactive"

        teamNumberLabel =
            String.concat [ "#", String.fromInt team.number ]

        defaultName =
            String.concat [ "Team ", String.fromInt team.number ]

        isDisabled =
            model.isSaving || model.isLocked
    in
    li [ class rowClass ]
        [ strong [ class "team-number" ] [ text teamNumberLabel ]
        , input
            [ type_ "text"
            , class "team-name-input"
            , value teamName
            , onInput (Page.SetTeamName team.number)
            , placeholder defaultName
            , disabled isDisabled
            ]
            []
        , button
            [ class "button small secondary"
            , onClick (Page.SaveTeamName team.number)
            , disabled isDisabled
            ]
            [ text "Save" ]
        , label [ class "toggle-label" ]
            [ input
                [ type_ "checkbox"
                , checked team.active
                , onCheck (Page.ToggleTeamActive team.number)
                , disabled isDisabled
                ]
                []
            , text "Active"
            ]
        ]


viewLockSection : Page.Model -> Html Page.Msg
viewLockSection model =
    let
        statusLabel =
            if model.isLocked then
                "Locked"

            else
                "Active"

        lockButton =
            if model.isLocked then
                if model.isAdmin then
                    button
                        [ class "button secondary"
                        , onClick Page.UnlockQuiz
                        , disabled model.isSaving
                        ]
                        [ text (saveButtonLabel model.isSaving "Unlock Quiz") ]

                else
                    text ""

            else
                button
                    [ class "button secondary"
                    , onClick Page.LockQuiz
                    , disabled model.isSaving
                    ]
                    [ text (saveButtonLabel model.isSaving "Lock Quiz") ]
    in
    section [ class "settings-section" ]
        [ h2 [] [ text "Quiz Status" ]
        , p [ class (String.concat [ "quiz-status ", statusLabel ]) ] [ text statusLabel ]
        , footer [ class "section-actions" ] [ lockButton ]
        ]


saveButtonLabel : Bool -> String -> String
saveButtonLabel isSaving defaultLabel =
    if isSaving then
        "Saving..."

    else
        defaultLabel
