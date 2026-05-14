module Pages.Public.QuizSheets.View exposing (view)

import Api.Types exposing (Quiz, QuizIdentifier, Round, Team)
import Date
import Html exposing (Html, article, aside, div, figcaption, figure, h1, h2, header, input, li, main_, ol, p, section, span, strong, text, time)
import Html.Attributes exposing (attribute, class, type_)
import Pages.Public.QuizSheets.Page as Page
import QRCode
import Result.Extra
import Util.Team


view : Page.Model -> Html Page.Msg
view model =
    if model.isLoading then
        section [ class "loading" ] [ p [] [ text "Loading..." ] ]

    else
        case model.error of
            Just err ->
                section [ class "error" ] [ p [] [ text err ] ]

            Nothing ->
                case model.quiz of
                    Nothing ->
                        section [ class "error" ] [ p [] [ text "Failed to load quiz." ] ]

                    Just quiz ->
                        viewSheets model.baseUrl quiz


type alias SheetPage =
    { team : Team
    , rounds : List Round
    , isFirstForTeam : Bool
    , isFirstOverall : Bool
    }


type Sheet
    = ContentSheet SheetPage
    | BlankSheet


viewSheets : String -> Quiz -> Html msg
viewSheets baseUrl quiz =
    let
        activeTeams =
            Util.Team.activeTeams quiz.scoreBoard.teams

        sortedRounds =
            quiz.rounds |> List.filter (\r -> r.numberOfQuestions > 0) |> List.sortBy .number

        roundPages =
            groupRoundsIntoPages sortedRounds

        needsBlankPage =
            modBy 2 (List.length roundPages) == 1

        teamSheets teamIndex team =
            let
                contentSheets =
                    roundPages
                        |> List.indexedMap
                            (\pageIndex rounds ->
                                ContentSheet
                                    { team = team
                                    , rounds = rounds
                                    , isFirstForTeam = pageIndex == 0
                                    , isFirstOverall = teamIndex == 0 && pageIndex == 0
                                    }
                            )
            in
            if needsBlankPage then
                contentSheets ++ [ BlankSheet ]
            else
                contentSheets

        sheets =
            activeTeams
                |> List.indexedMap teamSheets
                |> List.concat

        quizId =
            quiz.summary.quizId
    in
    main_ [ class "quiz-sheets" ]
        (viewOrganiserNote :: List.map (viewSheet baseUrl quizId quiz.summary.identifier) sheets)


viewOrganiserNote : Html msg
viewOrganiserNote =
    aside [ class "organiser-note" ]
        [ strong [] [ text "Organiser: " ]
        , text "Answer lines equal the number of questions configured per round. "
        , text "Press Ctrl+P / Cmd+P to print or save as PDF."
        ]


viewSheet : String -> Int -> QuizIdentifier -> Sheet -> Html msg
viewSheet baseUrl quizId identifier sheet =
    case sheet of
        BlankSheet ->
            article [ class "sheet new-page" ] []

        ContentSheet page ->
            let
                sheetClass =
                    if page.isFirstOverall then
                        "sheet"

                    else
                        "sheet new-page"
            in
            article [ class sheetClass ]
                (viewSheetHeader baseUrl quizId identifier page
                    :: List.map viewRoundSection page.rounds
                )


viewSheetHeader : String -> Int -> QuizIdentifier -> SheetPage -> Html msg
viewSheetHeader baseUrl quizId identifier page =
    if page.isFirstForTeam then
        let
            teamUrl =
                String.concat [ baseUrl, "/quizzes/", String.fromInt quizId, "/teams/", String.fromInt page.team.number ]

            quizLabel =
                String.concat [ identifier.name, " — ", identifier.place ]

            dateIso =
                Date.format "yyyy-MM-dd" identifier.date

            dateFormatted =
                Date.format "d MMMM y" identifier.date
        in
        header [ class "sheet-header" ]
            [ div [ class "sheet-hgroup" ]
                [ h1 [] [ text (Util.Team.teamName page.team) ]
                , p [] [ text quizLabel ]
                , time [ attribute "datetime" dateIso ] [ text dateFormatted ]
                ]
            , figure [ class "qr-code" ]
                [ QRCode.fromString teamUrl
                    |> Result.Extra.unwrap (text "") (QRCode.toSvg [])
                , figcaption [] [ text "Scan for live scores" ]
                ]
            ]

    else
        header [ class "sheet-header-minimal" ]
            [ h1 [] [ text (Util.Team.teamName page.team) ]
            ]


viewRoundSection : Round -> Html msg
viewRoundSection round =
    section [ class "round-section" ]
        [ header [ class "round-section-header" ]
            [ h2 [] [ text (String.concat [ "Round ", String.fromInt round.number ]) ] ]
        , ol [ class "answer-lines" ]
            (List.range 1 round.numberOfQuestions |> List.map viewAnswerLine)
        ]


viewAnswerLine : Int -> Html msg
viewAnswerLine n =
    li [ attribute "value" (String.fromInt n) ]
        [ span [ class "answer-space" ] []
        , input [ type_ "checkbox" ] []
        ]


groupRoundsIntoPages : List Round -> List (List Round)
groupRoundsIntoPages rounds =
    let
        step round ( currentPage, done ) =
            let
                pageRoundCount =
                    List.length currentPage

                pageLineCount =
                    List.sum (List.map .numberOfQuestions currentPage)
            in
            if pageRoundCount == 0 then
                ( [ round ], done )

            else if pageRoundCount == 1 && pageLineCount + round.numberOfQuestions <= 16 then
                ( List.concat [ currentPage, [ round ] ], done )

            else
                ( [ round ], List.concat [ done, [ currentPage ] ] )

        ( lastPage, completedPages ) =
            List.foldl step ( [], [] ) rounds
    in
    if List.isEmpty lastPage then
        completedPages

    else
        List.concat [ completedPages, [ lastPage ] ]
