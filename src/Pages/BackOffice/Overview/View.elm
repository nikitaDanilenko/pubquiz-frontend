module Pages.BackOffice.Overview.View exposing (view)

import Api.Types exposing (QuizIdentifier, QuizSummary)
import Date
import Html exposing (Html, a, button, h1, h2, li, p, section, text, ul)
import Html.Attributes exposing (class, disabled, href)
import Pages.BackOffice.Overview.Page as Page
import Util.Tristate as Tristate


view : Page.Model -> Html Page.Msg
view model =
    Tristate.fold
        { onInitial = viewLoading
        , onReady = viewOverview
        , onFailed = viewError
        }
        model.quizzes


viewLoading : Html msg
viewLoading =
    section [ class "loading" ]
        [ p [] [ text "Loading quizzes..." ]
        ]


viewError : a -> Html msg
viewError _ =
    section [ class "error" ]
        [ h1 [] [ text "Error" ]
        , p [] [ text "Failed to load quizzes." ]
        ]


viewOverview : List QuizSummary -> Html Page.Msg
viewOverview quizzes =
    let
        ( active, locked ) =
            List.partition .active quizzes

        sortedActive =
            active |> List.sortBy (\q -> Date.toRataDie q.identifier.date) |> List.reverse

        sortedLocked =
            locked |> List.sortBy (\q -> Date.toRataDie q.identifier.date) |> List.reverse
    in
    section [ class "backoffice-overview" ]
        [ h1 [] [ text "Back Office" ]
        , viewCreateQuizButton
        , viewQuizSection "Active Quizzes" sortedActive
        , viewQuizSection "Locked Quizzes" sortedLocked
        ]


viewCreateQuizButton : Html msg
viewCreateQuizButton =
    button [ class "create-quiz-button", disabled True ]
        [ text "Create Quiz" ]


viewQuizSection : String -> List QuizSummary -> Html msg
viewQuizSection title quizzes =
    section [ class "quiz-section" ]
        [ h2 [] [ text title ]
        , if List.isEmpty quizzes then
            p [ class "empty-section" ] [ text "No quizzes" ]

          else
            ul [ class "quiz-list" ]
                (quizzes |> List.map viewQuizItem)
        ]


viewQuizItem : QuizSummary -> Html msg
viewQuizItem quiz =
    li [ class "quiz-item" ]
        [ a [ href (quizUrl quiz.quizId) ]
            [ text (quizLabel quiz.identifier)
            ]
        ]


quizUrl : Int -> String
quizUrl quizId =
    String.concat [ "/backoffice/", String.fromInt quizId ]


quizLabel : QuizIdentifier -> String
quizLabel identifier =
    String.concat
        [ identifier.name
        , " ("
        , identifier.place
        , ", "
        , Date.format "d MMM y" identifier.date
        , ")"
        ]
