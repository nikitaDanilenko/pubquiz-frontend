module Common.Copy exposing (..)

import Common.Types exposing (Activity, DbQuizId, Header, Labels, Place, QuestionsInQuiz, QuizDate, QuizIdentifier, QuizInfo, QuizName, QuizRatings, QuizSettings, Ratings, TeamInfo, TeamName, TeamRating)
import Date exposing (Date)
import List.Extra exposing (setIf)


type LabelsField
    = RoundField
    | TeamField
    | OwnPointsField
    | MaxReachedField
    | MaxReachableField
    | BackField
    | OwnPageField
    | ViewPreviousField
    | CumulativeField
    | IndividualRoundsField
    | ProgressionField
    | PlacementField
    | PlaceField
    | PointsField
    | RoundWinnerField


updateLabelsByField : Labels -> LabelsField -> String -> Labels
updateLabelsByField labels field text =
    case field of
        RoundField ->
            { labels | roundLabel = text }

        TeamField ->
            { labels | teamLabel = text }

        OwnPointsField ->
            { labels | ownPointsLabel = text }

        MaxReachedField ->
            { labels | maxReachedLabel = text }

        MaxReachableField ->
            { labels | maxReachableLabel = text }

        BackField ->
            { labels | backToChartView = text }

        OwnPageField ->
            { labels | ownPageLabel = text }

        ViewPreviousField ->
            { labels | viewPrevious = text }

        CumulativeField ->
            { labels | cumulativeLabel = text }

        IndividualRoundsField ->
            { labels | individualRoundsLabel = text }

        ProgressionField ->
            { labels | progressionLabel = text }

        PlacementField ->
            { labels | placementLabel = text }

        PlaceField ->
            { labels | placeLabel = text }

        PointsField ->
            { labels | pointsLabel = text }

        RoundWinnerField ->
            { labels | roundWinnerLabel = text }


updateQuizIdentifierPlace : QuizIdentifier -> Place -> QuizIdentifier
updateQuizIdentifierPlace q p =
    { q | place = p }


updateQuizIdentifierDate : QuizIdentifier -> Date -> QuizIdentifier
updateQuizIdentifierDate q d =
    { q | date = Date.toIsoString d }


updateQuizIdentifierName : QuizIdentifier -> QuizName -> QuizIdentifier
updateQuizIdentifierName q n =
    { q | name = n }


updateQuizSettingsQuestionsInQuiz : QuizSettings -> QuestionsInQuiz -> QuizSettings
updateQuizSettingsQuestionsInQuiz qs questionsInQuiz =
    { qs | questionsInQuiz = questionsInQuiz }


updateQuizSettingsNumberOfTeams : QuizSettings -> Int -> QuizSettings
updateQuizSettingsNumberOfTeams qs ts =
    { qs | numberOfTeams = ts }


updateQuizSettingsLabels : QuizSettings -> Labels -> QuizSettings
updateQuizSettingsLabels qs ls =
    { qs | labels = ls }


updateQuizInfoQuizId : QuizInfo -> DbQuizId -> QuizInfo
updateQuizInfoQuizId qi qid =
    { qi | quizId = qid }


updateQuizInfoQuizIdentifier : QuizInfo -> QuizIdentifier -> QuizInfo
updateQuizInfoQuizIdentifier qi idf =
    { qi | quizIdentifier = idf }


updateQuizInfoQuizActivity : QuizInfo -> Activity -> QuizInfo
updateQuizInfoQuizActivity qi a =
    { qi | active = a }


updateHeaderTeamInfo : Header -> TeamInfo -> Header
updateHeaderTeamInfo header teamInfo =
    setIf (\ti -> ti.teamInfoNumber == teamInfo.teamInfoNumber) teamInfo header


updateTeamInfoActivity : TeamInfo -> Activity -> TeamInfo
updateTeamInfoActivity teamInfo activity =
    { teamInfo | teamInfoActivity = activity }


updateTeamInfoTeamName : TeamInfo -> TeamName -> TeamInfo
updateTeamInfoTeamName teamInfo teamName =
    { teamInfo | teamInfoName = teamName }


updateTeamRatingPoints : TeamRating -> Float -> TeamRating
updateTeamRatingPoints teamRating rating =
    { teamRating | rating = rating }
