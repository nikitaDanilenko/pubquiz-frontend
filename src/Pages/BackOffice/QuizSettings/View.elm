module Pages.BackOffice.QuizSettings.View exposing (view)

{-| Quiz Settings page view.
-}

import Api.Types exposing (QuizActive, Team)
import Dict exposing (Dict)
import Html exposing (Html, a, button, div, footer, h1, h2, header, input, label, li, nav, p, section, span, text, ul)
import Html.Attributes exposing (checked, class, disabled, for, href, id, placeholder, type_, value)
import Html.Events exposing (onCheck, onClick, onInput)
import Pages.BackOffice.QuizSettings.Page as Page


view : Page.Model -> Html Page.Msg
view model =
    section [ class "quiz-settings" ]
        [ viewHeader model
        , viewContent model
        ]


viewHeader : Page.Model -> Html Page.Msg
viewHeader model =
    header [ class "quiz-settings-header" ]
        [ h1 []
            [ text
                (model.quiz
                    |> Maybe.map (\q -> String.concat [ q.identifier.name, " — Settings" ])
                    |> Maybe.withDefault "Loading..."
                )
            ]
        , nav [ class "quiz-settings-actions" ]
            [ a
                [ href (String.concat [ "/backoffice/", String.fromInt model.quizId ])
                , class "button primary"
                ]
                [ text "Point Entry" ]
            , a [ href "/backoffice", class "button secondary" ] [ text "Back" ]
            ]
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
                    , viewIdentifierSection model
                    , viewTeamsSection model quiz
                    , viewAddTeamsSection model
                    ]

            Nothing ->
                p [ class "error" ] [ text "Failed to load quiz" ]


viewMessages : Page.Model -> Html msg
viewMessages model =
    div [ class "messages" ]
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
            [ div [ class "form-field" ]
                [ label [ for "name" ] [ text "Name" ]
                , input
                    [ type_ "text"
                    , id "name"
                    , value model.name
                    , onInput Page.SetName
                    , disabled model.isSaving
                    ]
                    []
                ]
            , div [ class "form-field" ]
                [ label [ for "date" ] [ text "Date" ]
                , input
                    [ type_ "date"
                    , id "date"
                    , value model.date
                    , onInput Page.SetDate
                    , disabled model.isSaving
                    ]
                    []
                ]
            , div [ class "form-field" ]
                [ label [ for "place" ] [ text "Place" ]
                , input
                    [ type_ "text"
                    , id "place"
                    , value model.place
                    , onInput Page.SetPlace
                    , disabled model.isSaving
                    ]
                    []
                ]
            ]
        , footer [ class "section-actions" ]
            [ button
                [ class "button primary"
                , onClick Page.SaveIdentifier
                , disabled model.isSaving
                ]
                [ text
                    (if model.isSaving then
                        "Saving..."

                     else
                        "Save Details"
                    )
                ]
            ]
        ]


viewTeamsSection : Page.Model -> QuizActive -> Html Page.Msg
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
    in
    li
        [ class
            (String.concat
                [ "team-row"
                , if team.active then
                    ""

                  else
                    " inactive"
                ]
            )
        ]
        [ span [ class "team-number" ]
            [ text (String.concat [ "#", String.fromInt team.number ]) ]
        , input
            [ type_ "text"
            , class "team-name-input"
            , value teamName
            , onInput (Page.SetTeamName team.number)
            , placeholder (String.concat [ "Team ", String.fromInt team.number ])
            , disabled model.isSaving
            ]
            []
        , button
            [ class "button small secondary"
            , onClick (Page.SaveTeamName team.number)
            , disabled model.isSaving
            ]
            [ text "Save" ]
        , label [ class "toggle-label" ]
            [ input
                [ type_ "checkbox"
                , checked team.active
                , onCheck (Page.ToggleTeamActive team.number)
                , disabled model.isSaving
                ]
                []
            , span [] [ text "Active" ]
            ]
        ]


viewAddTeamsSection : Page.Model -> Html Page.Msg
viewAddTeamsSection model =
    section [ class "settings-section" ]
        [ h2 [] [ text "Add Teams" ]
        , div [ class "add-teams-form" ]
            [ input
                [ type_ "number"
                , value (String.fromInt model.additionalTeams)
                , onInput Page.SetAdditionalTeams
                , Html.Attributes.min "1"
                , disabled model.isSaving
                ]
                []
            , button
                [ class "button primary"
                , onClick Page.AddTeams
                , disabled model.isSaving
                ]
                [ text
                    (if model.isSaving then
                        "Adding..."

                     else
                        String.concat [ "Add ", String.fromInt model.additionalTeams, " Team(s)" ]
                    )
                ]
            ]
        ]
