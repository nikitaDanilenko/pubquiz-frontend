module Pages.BackOffice.CreateQuiz.View exposing (view)

import Html exposing (Html, button, form, h1, input, label, p, section, text)
import Html.Attributes as Attr exposing (class, disabled, for, id, placeholder, type_, value)
import Html.Events exposing (onInput, onSubmit)
import Pages.BackOffice.CreateQuiz.Page as Page


view : Page.Model -> Html Page.Msg
view model =
    section [ class "create-quiz" ]
        [ h1 [] [ text "Create Quiz" ]
        , viewForm model
        , viewError model.error
        ]


viewForm : Page.Model -> Html Page.Msg
viewForm model =
    form [ class "create-quiz-form", onSubmit Page.Submit ]
        [ viewTextField "name" "Quiz Name" model.name Page.SetName model.isSubmitting
        , viewDateField "date" "Date" model.date Page.SetDate model.isSubmitting
        , viewTextField "place" "Place" model.place Page.SetPlace model.isSubmitting
        , viewNumberField "rounds" "Number of Rounds" model.numberOfRounds Page.SetNumberOfRounds model.isSubmitting
        , viewQuestionsPerRound model
        , viewNumberField "teams" "Number of Teams" model.numberOfTeams Page.SetNumberOfTeams model.isSubmitting
        , button
            [ type_ "submit"
            , class "submit-button"
            , disabled (model.isSubmitting || not (isValid model))
            ]
            [ text (buttonText model) ]
        ]


viewTextField : String -> String -> String -> (String -> Page.Msg) -> Bool -> Html Page.Msg
viewTextField fieldId labelText fieldValue toMsg isDisabled =
    section [ class "form-field" ]
        [ label [ for fieldId ] [ text labelText ]
        , input
            [ type_ "text"
            , id fieldId
            , placeholder labelText
            , value fieldValue
            , onInput toMsg
            , disabled isDisabled
            ]
            []
        ]


viewDateField : String -> String -> String -> (String -> Page.Msg) -> Bool -> Html Page.Msg
viewDateField fieldId labelText fieldValue toMsg isDisabled =
    section [ class "form-field" ]
        [ label [ for fieldId ] [ text labelText ]
        , input
            [ type_ "date"
            , id fieldId
            , value fieldValue
            , onInput toMsg
            , disabled isDisabled
            ]
            []
        ]


viewNumberField : String -> String -> Int -> (String -> Page.Msg) -> Bool -> Html Page.Msg
viewNumberField fieldId labelText fieldValue toMsg isDisabled =
    section [ class "form-field" ]
        [ label [ for fieldId ] [ text labelText ]
        , input
            [ type_ "number"
            , id fieldId
            , value (String.fromInt fieldValue)
            , onInput toMsg
            , disabled isDisabled
            , Attr.min "1"
            ]
            []
        ]


viewQuestionsPerRound : Page.Model -> Html Page.Msg
viewQuestionsPerRound model =
    section [ class "form-field questions-per-round" ]
        [ label [] [ text "Questions per Round" ]
        , section [ class "round-inputs" ]
            (model.questionsPerRound
                |> List.indexedMap (viewRoundInput model.isSubmitting)
            )
        ]


viewRoundInput : Bool -> Int -> Int -> Html Page.Msg
viewRoundInput isDisabled index questionCount =
    section [ class "round-input" ]
        [ label [ for (roundId index) ]
            [ text (String.concat [ "Round ", String.fromInt (index + 1) ]) ]
        , input
            [ type_ "number"
            , id (roundId index)
            , value (String.fromInt questionCount)
            , onInput (Page.SetQuestionsForRound index)
            , disabled isDisabled
            , Attr.min "0"
            ]
            []
        ]


roundId : Int -> String
roundId index =
    String.concat [ "round-", String.fromInt index ]


isValid : Page.Model -> Bool
isValid model =
    List.all identity
        [ not (String.isEmpty model.name)
        , not (String.isEmpty model.date)
        , not (String.isEmpty model.place)
        , model.numberOfRounds > 0
        , List.length model.questionsPerRound == model.numberOfRounds
        , model.numberOfTeams > 0
        ]


buttonText : Page.Model -> String
buttonText model =
    if model.isSubmitting then
        "Creating..."

    else
        "Create Quiz"


viewError : Maybe String -> Html msg
viewError maybeError =
    case maybeError of
        Just error ->
            p [ class "form-error" ] [ text error ]

        Nothing ->
            text ""
