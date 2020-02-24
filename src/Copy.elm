module Copy exposing (..)

import Types exposing (Activity, DbQuizId, Labels, Place, QuizDate, QuizInfo, QuizName, QuizIdentifier, QuizSettings)

type LabelsField = RoundField
                 | TeamField
                 | OwnPointsField
                 | MaxReachedField
                 | MaxReachableField
                 | BackField
                 | MainField
                 | OwnPageField
                 | ViewPreviousField
                 | CumulativeField
                 | IndividualRoundsField
                 | ProgressionField
                 | PlacementField
                 | PlaceField
                 | PointsField
                 | RoundWinnerField

updateLabelsByField : LabelsField -> String -> Labels -> Labels
updateLabelsByField field text lbls =
    case field of
        RoundField -> { lbls | roundLabel = text }
        TeamField -> { lbls | teamLabel = text }
        OwnPointsField -> { lbls | ownPointsLabel = text }
        MaxReachedField -> { lbls | maxReachedLabel = text }
        MaxReachableField -> { lbls | maxReachableLabel = text }
        BackField -> { lbls | backToChartView = text }
        MainField -> { lbls | mainLabel = text }
        OwnPageField -> { lbls | ownPageLabel = text }
        ViewPreviousField -> { lbls | viewPrevious = text }
        CumulativeField -> { lbls | cumulativeLabel = text }
        IndividualRoundsField -> { lbls | individualRoundsLabel = text }
        ProgressionField -> { lbls | progressionLabel = text }
        PlacementField -> { lbls | placementLabel = text }
        PlaceField -> { lbls | placeLabel = text }
        PointsField -> { lbls | pointsLabel = text }
        RoundWinnerField -> { lbls | roundWinnerLabel = text }

updateQuizIdentifierPlace : QuizIdentifier -> Place -> QuizIdentifier
updateQuizIdentifierPlace q p = { q | place = p }

updateQuizIdentifierDate : QuizIdentifier -> QuizDate -> QuizIdentifier
updateQuizIdentifierDate q d = { q | date = d }

updateQuizIdentifierName : QuizIdentifier -> QuizName -> QuizIdentifier
updateQuizIdentifierName q n = { q | name = n }

updateQuizSettingsRounds : QuizSettings -> List Int -> QuizSettings
updateQuizSettingsRounds qs rs = { qs | rounds = rs }

updateQuizSettingsNumberOfTeams : QuizSettings -> Int -> QuizSettings
updateQuizSettingsNumberOfTeams qs ts = { qs | numberOfTeams = ts }

updateQuizSettingsLabels : QuizSettings -> Labels -> QuizSettings
updateQuizSettingsLabels qs ls = { qs | labels = ls }

updateQuizInfoQuizId : QuizInfo -> DbQuizId -> QuizInfo
updateQuizInfoQuizId qi qid = { qi | quizId = qid }

updateQuizInfoQuizIdentifier : QuizInfo -> QuizIdentifier -> QuizInfo
updateQuizInfoQuizIdentifier qi pdn = { qi | quizIdentifier = pdn }

updateQuizInfoQuizActivity : QuizInfo -> Activity -> QuizInfo
updateQuizInfoQuizActivity qi a = { qi | active = a }