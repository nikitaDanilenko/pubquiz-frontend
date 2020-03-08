module Output.Quiz exposing (..)

import Chartjs.Chart exposing (chart)
import Common.Constants exposing (getLabelsApi, getQuizInfoApi, getQuizRatingsApi)
import Common.QuizRatings as QuizRatings
import Common.Ranking exposing (RoundRankings, rankingToPlacement, ratingsToRankings, roundRankingsToRoundWinners)
import Common.Types exposing (DbQuizId, Labels, QuizInfo, QuizRatings, TeamQuery, jsonDecLabels, jsonDecQuizInfo, jsonDecQuizRatings)
import Common.Util as Util exposing (getMsg)
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (class, href, id)
import Html.Events exposing (onClick)
import Input.Model as Input exposing (ErrorOr)
import List.Extra exposing (maximumBy)
import Output.Charts as Charts
import Output.Colors exposing (mkColors)


type alias Model =
    { labels : Labels
    , teamQueryCandidate : Maybe TeamQuery
    , quizRatings : QuizRatings
    , quizInfo : QuizInfo
    }

updateLabels : Model -> Labels -> Model
updateLabels model labels = {model | labels = labels}

updateQuizRatings : Model -> QuizRatings -> Model
updateQuizRatings model quizRatings = {model | quizRatings = quizRatings}

updateQuizInfo : Model -> QuizInfo -> Model
updateQuizInfo model quizInfo = {model | quizInfo = quizInfo}


type Msg
    = GetTeamTable TeamQuery
    | GetQuizRatings DbQuizId
    | GotQuizRatings (ErrorOr QuizRatings)
    | GotLabels (ErrorOr Labels)
    | GotQuizInfo (ErrorOr QuizInfo)


init : DbQuizId -> ( Model, Cmd Msg )
init qid =
    ( { labels = Input.defaultLabels,
    teamQueryCandidate = Nothing,
    quizRatings = QuizRatings.empty,
    quizInfo = Input.defaultQuizInfo },
    Cmd.batch [getQuizRatings qid, getQuizInfo qid, getLabels qid] )


view : Model -> Html Msg
view model =
    let
        rankings =
            ratingsToRankings model.quizRatings.ratings model.quizRatings.header

        roundLabels =
            List.map (\( n, _ ) -> String.join " " [ model.labels.roundLabel, String.fromInt n ]) rankings.sortedRatings

        backToTable =
            case model.teamQueryCandidate of
                Just teamQuery ->
                    [ div [ id "backToTable" ]
                        [ button [ class "ownPointsButton", onClick (GetTeamTable teamQuery) ]
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
                    [ chart 800 600 (Charts.cumulativeChart colors rankings.cumulative roundLabels model.labels.cumulativeLabel) ]
               , div [ id "perRoundChart" ]
                    [ chart 800 600 (Charts.perRoundChart colors rankings.perRound roundLabels model.labels.individualRoundsLabel) ]
               , div [ id "progressionChart" ]
                    [ chart 800 600 (Charts.progressionChart colors rankings.cumulative roundLabels model.labels.progressionLabel) ]
               , div [ id "allQuizzes" ]
                    [ button [ class "allQuizzesButton",
                               href "/"
                               {-, onClick GetAllQuizzes -}
                             ] [ text model.labels.viewPrevious ] ]
               ]
            ++ backToTable
        )

update : Msg -> Model -> ( Model, Cmd Msg)
update msg model =
  case msg of
    GotQuizRatings quizRatingsCandidate ->
      (Util.foldResult model (updateQuizRatings model) quizRatingsCandidate, Cmd.none)

    GotLabels labelsCandidate ->
      (Util.foldResult model (updateLabels model) labelsCandidate, Cmd.none)

    GotQuizInfo quizInfoCandidate ->
      (Util.foldResult model (updateQuizInfo model) quizInfoCandidate, Cmd.none)

    _ -> (model, Cmd.none)

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
getLabels = getMsg getLabelsApi GotLabels jsonDecLabels

getQuizInfo : DbQuizId -> Cmd Msg
getQuizInfo =
    getMsg getQuizInfoApi GotQuizInfo jsonDecQuizInfo