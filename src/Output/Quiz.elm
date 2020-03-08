module Output.Quiz exposing (Model, Msg, init, update, view)

import Chartjs.Chart exposing (chart)
import Common.ConnectionUtil exposing (getLabelsWith, getQuizInfoWith, getQuizRatingsWith, linkButton, useOrFetchWith)
import Common.Constants exposing (quizIdParam, teamCodeParam, teamNumberParam)
import Common.QuizRatings as QuizRatings
import Common.Ranking exposing (RoundRankings, rankingToPlacement, ratingsToRankings, roundRankingsToRoundWinners)
import Common.Types exposing (DbQuizId, Labels, QuizInfo, QuizRatings, TeamQuery)
import Common.Util as Util
import Html exposing (Html, div, label, text)
import Html.Attributes exposing (class, for, id, value)
import Input.Model as Input exposing (ErrorOr)
import List.Extra exposing (maximumBy)
import Output.Charts as Charts
import Output.Colors exposing (mkColors)
import Output.OutputUtil exposing (fragmentUrl, mkFullQuizName)


type alias Model =
    { labels : Labels
    , teamQueryCandidate : Maybe TeamQuery
    , quizRatings : QuizRatings
    , quizInfo : QuizInfo
    , status : Status
    }


updateLabels : Model -> Labels -> Model
updateLabels model labels =
    { model | labels = labels, status = updateLabelsSet model.status True }


updateQuizRatings : Model -> QuizRatings -> Model
updateQuizRatings model quizRatings =
    { model | quizRatings = quizRatings, status = updateQuizRatingsSet model.status True }


updateQuizInfo : Model -> QuizInfo -> Model
updateQuizInfo model quizInfo =
    { model | quizInfo = quizInfo, status = updateQuizInfoSet model.status True }


{-| Without the status, the charts are initialised with whatever values are contained in the initial
models only. The precise background is unclear.
Using a status is a simple workaround since we only create charts once every value has been set.
-}
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


type Msg
    = GotQuizRatings (ErrorOr QuizRatings)
    | GotLabels (ErrorOr Labels)
    | GotQuizInfo (ErrorOr QuizInfo)


init : Maybe Labels -> Maybe TeamQuery -> DbQuizId -> ( Model, Cmd Msg )
init mLabels mTeamQuery qid =
    ( { labels = Maybe.withDefault Input.defaultLabels mLabels
      , teamQueryCandidate = mTeamQuery
      , quizRatings = QuizRatings.empty
      , quizInfo = Input.defaultQuizInfo
      , status = { loading | labelsSet = Util.isDefined mLabels }
      }
    , Cmd.batch
        [ getQuizInfoWith GotQuizInfo qid
        , useOrFetchWith (getLabelsWith GotLabels) mLabels qid
        , getQuizRatingsWith GotQuizRatings qid
        ]
    )


view : Model -> Html Msg
view model =
    if not (isFinished model.status) then
        -- todo add loading symbol
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
                        if teamQuery.teamQueryQuizId == model.quizInfo.quizId then
                            [ div [ id "backToTable" ]
                                [ linkButton
                                    (mkTeamQueryLink teamQuery)
                                    [ class "ownPointsButton", value model.labels.ownPointsLabel ]
                                    []
                                ]
                            ]

                        else
                            []

                    Nothing ->
                        []

            colors =
                mkColors (List.length model.quizRatings.header)
        in
        div [ id "quizView" ]
            (div [ id "quizTitle" ] [ label [ for "quizTitleLabel" ] [ text (mkFullQuizName model.quizInfo.quizIdentifier) ] ]
                :: div [ id "rankings" ]
                    [ mkPlacements rankings.cumulative model.labels.placementLabel model.labels.placeLabel model.labels.pointsLabel
                    , mkRoundWinners rankings.perRound model.labels.roundWinnerLabel model.labels.roundLabel model.labels.pointsLabel
                    ]
                :: div [ id "charts" ]
                    [ div [ id "cumulativeChart" ]
                        [ chart 800 600 cumulativeChart ]
                    , div [ id "perRoundChart" ]
                        [ chart 800 600 perRoundChart ]
                    , div [ id "progressionChart" ]
                        [ chart 800 600 progressionChart ]
                    ]
                :: div [ id "allQuizzes" ]
                    [ linkButton
                        (fragmentUrl [ "" ])
                        [ class "allQuizzesButton", value model.labels.viewPrevious ]
                        []
                    ]
                :: backToTable
            )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        newModel =
            case msg of
                GotQuizRatings quizRatingsCandidate ->
                    Util.foldResult model (updateQuizRatings model) quizRatingsCandidate

                GotLabels labelsCandidate ->
                    Util.foldResult model (updateLabels model) labelsCandidate

                GotQuizInfo quizInfoCandidate ->
                    Util.foldResult model (updateQuizInfo model) quizInfoCandidate
    in
    ( newModel, Cmd.none )


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
        (label [ for "placementsLabel" ] [ text wordForPlacement ]
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
        (label [ for "roundWinnersLabel" ] [ text wordForRoundWinner ]
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


mkTeamQueryLink : TeamQuery -> String
mkTeamQueryLink teamQuery =
    fragmentUrl
        [ quizIdParam
        , String.fromInt teamQuery.teamQueryQuizId
        , teamNumberParam
        , String.fromInt teamQuery.teamQueryTeamNumber
        , teamCodeParam
        , teamQuery.teamQueryTeamCode
        ]
