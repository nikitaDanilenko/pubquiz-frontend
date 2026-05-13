module Pages.BackOffice.QuizSettings.View exposing (view)

import Api.Types exposing (Quiz, Round, Team)
import Dict exposing (Dict)
import Html exposing (Html, a, button, div, footer, h1, h2, header, input, label, li, nav, p, section, span, text, ul)
import Html.Attributes as Attr exposing (checked, class, disabled, for, href, id, placeholder, type_, value)
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
                    , viewRoundsSection model quiz
                    , viewTeamsSection model quiz
                    ]

            Nothing ->
                p [ class "error" ] [ text "Failed to load quiz" ]


viewMessages : Page.Model -> Html msg
viewMessages model =
    case ( model.error, model.successMessage ) of
        ( Nothing, Nothing ) ->
            text ""

        _ ->
            section [ class "messages" ]
                [ case model.error of
                    Just error ->
                        p [ class "form-error" ] [ text error ]

                    Nothing ->
                        text ""
                , case model.successMessage of
                    Just msg ->
                        p [ class "form-success" ] [ text msg ]

                    Nothing ->
                        text ""
                ]


viewIdentifierSection : Page.Model -> Html Page.Msg
viewIdentifierSection model =
    section [ class "settings-section" ]
        [ h2 [] [ text "Quiz Details" ]
        , div [ class "form-fields" ]
            [ p [ class "form-field" ]
                [ label [ for "name" ] [ text "Name" ]
                , input
                    [ type_ "text"
                    , id "name"
                    , value model.name
                    , onInput Page.SetName
                    , disabled (model.isSaving || model.isLocked)
                    ]
                    []
                ]
            , p [ class "form-field" ]
                [ label [ for "date" ] [ text "Date" ]
                , input
                    [ type_ "date"
                    , id "date"
                    , value model.date
                    , onInput Page.SetDate
                    , disabled (model.isSaving || model.isLocked)
                    ]
                    []
                ]
            , p [ class "form-field" ]
                [ label [ for "place" ] [ text "Place" ]
                , input
                    [ type_ "text"
                    , id "place"
                    , value model.place
                    , onInput Page.SetPlace
                    , disabled (model.isSaving || model.isLocked)
                    ]
                    []
                ]
            ]
        , footer [ class "section-actions" ]
            [ button
                [ class "button primary"
                , onClick Page.SaveIdentifier
                , disabled (model.isSaving || model.isLocked)
                ]
                [ text (saveButtonLabel model.isSaving "Save Details") ]
            ]
        ]


viewRoundsSection : Page.Model -> Quiz -> Html Page.Msg
viewRoundsSection model quiz =
    section [ class "settings-section" ]
        [ h2 [] [ text "Questions per Round" ]
        , section [ class "round-inputs" ]
            (quiz.rounds
                |> List.sortBy .number
                |> List.map (viewRoundRow model)
            )
        , footer [ class "section-actions" ]
            [ button
                [ class "button primary"
                , onClick Page.SaveIdentifier
                , disabled (model.isSaving || model.isLocked)
                ]
                [ text (saveButtonLabel model.isSaving "Save Rounds") ]
            ]
        ]


viewRoundRow : Page.Model -> Round -> Html Page.Msg
viewRoundRow model round =
    let
        rowId =
            String.concat [ "round-", String.fromInt round.number ]

        questionCount =
            Dict.get round.number model.questionsPerRound
                |> Maybe.withDefault round.numberOfQuestions
    in
    section [ class "round-input" ]
        [ label [ for rowId ]
            [ text (String.concat [ "Round ", String.fromInt round.number ]) ]
        , input
            [ type_ "number"
            , id rowId
            , value (String.fromInt questionCount)
            , onInput (Page.SetQuestionsForRound round.number)
            , Attr.min "0"
            , disabled (model.isSaving || model.isLocked)
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
    in
    li [ class rowClass ]
        [ span [ class "team-number" ]
            [ text (String.concat [ "#", String.fromInt team.number ]) ]
        , input
            [ type_ "text"
            , class "team-name-input"
            , value teamName
            , onInput (Page.SetTeamName team.number)
            , placeholder (String.concat [ "Team ", String.fromInt team.number ])
            , disabled (model.isSaving || model.isLocked)
            ]
            []
        , button
            [ class "button small secondary"
            , onClick (Page.SaveTeamName team.number)
            , disabled (model.isSaving || model.isLocked)
            ]
            [ text "Save" ]
        , label [ class "toggle-label" ]
            [ input
                [ type_ "checkbox"
                , checked team.active
                , onCheck (Page.ToggleTeamActive team.number)
                , disabled (model.isSaving || model.isLocked)
                ]
                []
            , span [] [ text "Active" ]
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


