module Pages.BackOffice.Shared exposing (NavTab(..), viewQuizNav)

import Html exposing (Html, a, nav)
import Html.Attributes exposing (class, href)


type NavTab
    = PointEntry
    | Sheets
    | Settings


viewQuizNav : Int -> NavTab -> Html msg
viewQuizNav quizId active =
    nav [ class "quiz-nav" ]
        [ navLink (active == PointEntry) (String.concat [ "/backoffice/", String.fromInt quizId ]) "Point Entry"
        , navLink (active == Settings) (String.concat [ "/backoffice/", String.fromInt quizId, "/settings" ]) "Settings"
        , navLink False (String.concat [ "/quizzes/", String.fromInt quizId ]) "Public View"
        , navLink (active == Sheets) (String.concat [ "/quizzes/", String.fromInt quizId, "/sheets" ]) "Print Sheets"
        , navLink False "/backoffice" "← Back"
        ]


navLink : Bool -> String -> String -> Html msg
navLink isActive url label =
    a
        [ href url
        , class
            (if isActive then
                "button primary"

             else
                "button secondary"
            )
        ]
        [ Html.text label ]
