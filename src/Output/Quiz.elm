module Output.Quiz exposing (..)

import Chartjs.Chart exposing (chart)
import Common.Constants exposing (getLabelsApi, getQuizInfoApi, getQuizRatingsApi)
import Common.QuizRatings as QuizRatings
import Common.Ranking exposing (RoundRankings, rankingToPlacement, ratingsToRankings, roundRankingsToRoundWinners)
import Common.Types exposing (DbQuizId, Labels, QuizInfo, QuizRatings, TeamQuery, jsonDecLabels, jsonDecQuizInfo, jsonDecQuizRatings)
import Common.Util as Util exposing (getMsg)
import Html exposing (Html, a, button, div, text)
import Html.Attributes exposing (class, href, id)
import Html.Events exposing (onClick)
import Input.Model as Input exposing (ErrorOr)
import List.Extra exposing (maximumBy)
import Output.Charts as Charts
import Output.Colors exposing (mkColors)
import Output.Model


type alias Model =
    { labels : Labels
    , teamQueryCandidate : Maybe TeamQuery
    , quizRatings : QuizRatings
    , quizInfo : QuizInfo
    , status : Status
    }



-- | Without the status, the charts are initialised with whatever values are contained in the initial
--   models only. The precise background is unclear.
--   Using a status is a simple workaround since we only create charts once every value has been set.


type alias Status =
    { labelsSet : Bool
    , quizRatingsSet : Bool
    , quizInfoSet : Bool
    }


loading : Status
loading =
    Status False False False


isFinished : Status -> Bool
isFinished s =
    List.all identity [ s.labelsSet, s.quizRatingsSet, s.quizInfoSet ]


updateLabelsSet : Status -> Bool -> Status
updateLabelsSet s b =
    { s | labelsSet = b }


updateQuizRatingsSet : Status -> Bool -> Status
updateQuizRatingsSet s b =
    { s | quizRatingsSet = b }


updateQuizInfoSet : Status -> Bool -> Status
updateQuizInfoSet s b =
    { s | quizInfoSet = b }


updateLabels : Model -> Labels -> Model
updateLabels model labels =
    { model | labels = labels, status = updateLabelsSet model.status True }


updateQuizRatings : Model -> QuizRatings -> Model
updateQuizRatings model quizRatings =
    { model | quizRatings = quizRatings, status = updateQuizRatingsSet model.status True }


updateQuizInfo : Model -> QuizInfo -> Model
updateQuizInfo model quizInfo =
    { model | quizInfo = quizInfo, status = updateQuizInfoSet model.status True }


type Msg
    = GotQuizRatings (ErrorOr QuizRatings)
    | GotLabels (ErrorOr Labels)
    | GotQuizInfo (ErrorOr QuizInfo)


init : DbQuizId -> ( Model, Cmd Msg )
init qid =
    ( { labels = Input.defaultLabels
      , teamQueryCandidate = Nothing
      , quizRatings = Output.Model.testRatings
      , quizInfo = Input.defaultQuizInfo
      , status = loading
      }
    , Cmd.batch [ getQuizInfo qid, getLabels qid, getQuizRatings qid ]
    )


view : Model -> Html Msg
view model =
    if not (isFinished model.status) then
        div [] []

    else
        let
            rankings =
                ratingsToRankings model.quizRatings.ratings model.quizRatings.header

            roundLabels =
                List.map (\( n, _ ) -> String.join " " [ model.labels.roundLabel, String.fromInt n ]) rankings.sortedRatings

            cumulativeChart =
                Charts.cumulativeChart colors rankings.cumulative roundLabels model.labels.cumulativeLabel

            perRoundChart =
                Charts.perRoundChart colors rankings.perRound roundLabels model.labels.individualRoundsLabel

            progressionChart =
                Charts.progressionChart colors rankings.cumulative roundLabels model.labels.progressionLabel

            backToTable =
                case model.teamQueryCandidate of
                    Just teamQuery ->
                        [ div [ id "backToTable" ]
                            [ button
                                [ class "ownPointsButton"

                                {- , onClick (GetTeamTable teamQuery) -}
                                ]
                                [ text model.labels.ownPointsLabel ]
                            ]
                        ]

                    Nothing ->
                        []

            colors =
                mkColors (List.length model.quizRatings.header)
        in
        div [ id "charts" ]
            (mkPlacements rankings.cumulative model.labels.placementLabel model.labels.placeLabel model.labels.pointsLabel
                :: mkRoundWinners rankings.perRound model.labels.roundWinnerLabel model.labels.roundLabel model.labels.pointsLabel
                :: [ div [ id "cumulativeChart" ]
                        [ chart 800 600 cumulativeChart ]
                   , div [ id "perRoundChart" ]
                        [ chart 800 600 perRoundChart ]
                   , div [ id "progressionChart" ]
                        [ chart 800 600 progressionChart ]
                   , div [ id "allQuizzes" ]
                        [ a
                            [ class "allQuizzesButton"
                            , href "/"

                            {- , onClick GetAllQuizzes -}
                            ]
                            [ text model.labels.viewPrevious ]
                        ]
                   ]
                ++ backToTable
            )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotQuizRatings quizRatingsCandidate ->
            ( Util.foldResult model (updateQuizRatings model) quizRatingsCandidate, Cmd.none )

        GotLabels labelsCandidate ->
            ( Util.foldResult model (updateLabels model) labelsCandidate, Cmd.none )

        GotQuizInfo quizInfoCandidate ->
            ( Util.foldResult model (updateQuizInfo model) quizInfoCandidate, Cmd.none )


mkPlacements : RoundRankings -> String -> String -> String -> Html Msg
mkPlacements rrs wordForPlacement wordForPlace wordForPoints =
    let
        zeroRating =
            ( 1, { teamNumber = 1, rating = 0 } )

        findCurrent =
            .teamRatings >> maximumBy Tuple.first >> Maybe.withDefault zeroRating >> Tuple.second

        currentRanking =
            List.map (\perTeam -> { teamName = perTeam.teamName, teamRating = findCurrent perTeam }) rrs

        placement =
            rankingToPlacement currentRanking
    in
    div [ id "placements" ]
        (text wordForPlacement
            :: List.map
                (\tr ->
                    div [ id "place" ]
                        [ text
                            (String.join " "
                                [ String.concat
                                    [ String.join " "
                                        [ wordForPlace
                                        , String.fromInt tr.position
                                        , String.concat [ "(", String.fromFloat tr.teamRating.rating, " ", wordForPoints, ")" ]
                                        ]
                                    , ":"
                                    ]
                                , tr.teamName
                                ]
                            )
                        ]
                )
                placement
        )


mkRoundWinners : RoundRankings -> String -> String -> String -> Html Msg
mkRoundWinners rr wordForRoundWinner wordForRound wordForPoints =
    let
        roundWinners =
            roundRankingsToRoundWinners rr
    in
    div [ id "roundWinners" ]
        (text wordForRoundWinner
            :: List.map
                (\rw ->
                    div [ id "roundWinner" ]
                        [ text
                            (String.concat
                                [ wordForRound
                                , " "
                                , String.fromInt rw.roundNumber
                                , " ("
                                , String.fromFloat rw.points
                                , " "
                                , wordForPoints
                                , "): "
                                , String.join ", " rw.teamNames
                                ]
                            )
                        ]
                )
                roundWinners
        )


getQuizRatings : DbQuizId -> Cmd Msg
getQuizRatings =
    getMsg getQuizRatingsApi GotQuizRatings jsonDecQuizRatings


getLabels : DbQuizId -> Cmd Msg
getLabels =
    getMsg getLabelsApi GotLabels jsonDecLabels


getQuizInfo : DbQuizId -> Cmd Msg
getQuizInfo =
    getMsg getQuizInfoApi GotQuizInfo jsonDecQuizInfo
