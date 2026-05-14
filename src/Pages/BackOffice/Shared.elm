module Pages.BackOffice.Shared exposing (NavTab(..), viewQuizNav)

import Html exposing (Html, a, nav)
import Html.Attributes exposing (class, href)


type NavTab
    = PointEntry
    | Sheets
    | Settings


viewQuizNav : Int -> NavTab -> Html msg
viewQuizNav quizId active =
    let
        id =
            String.fromInt quizId

        backofficeUrl =
            "/backoffice/" ++ id

        publicUrl =
            "/quizzes/" ++ id
    in
    nav [ class "quiz-nav" ]
        [ navLink (active == PointEntry) backofficeUrl "Point Entry"
        , navLink (active == Settings) (backofficeUrl ++ "/settings") "Settings"
        , navLink False publicUrl "Public View"
        , navLink (active == Sheets) (publicUrl ++ "/sheets") "Print Sheets"
        , navLink False "/backoffice" "← Back"
        ]


navLink : Bool -> String -> String -> Html msg
navLink isActive url label =
    let
        buttonClass =
            if isActive then
                "button primary"

            else
                "button secondary"
    in
    a [ href url, class buttonClass ] [ Html.text label ]
