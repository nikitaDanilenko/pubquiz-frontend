module Output.Table exposing (Model, Msg, init, update, view)

import Basics.Extra exposing (uncurry)
import Color.Convert
import Common.Constants exposing (getQuizRatingsApi, quizIdParam)
import Common.QuizRatings as QuizRatings exposing (cumulative)
import Common.Types exposing (DbQuizId, Labels, QuizInfo, QuizRatings, Ratings, RoundNumber, RoundRating, TeamLine, TeamNumber, TeamQuery, TeamRating, TeamTable, TeamTableInfo, jsonDecQuizRatings)
import Common.Util as Util exposing (ErrorOr, getMsg)
import Common.WireUtil exposing (getLabelsWith, getQuizInfoWith, linkButton, loadingSymbol, useOrFetchWith)
import Html exposing (Html, div, h1, label, table, td, text, th, tr)
import Html.Attributes exposing (class, for, id, style, value)
import Input.QuizValues as QuizValues
import List.Extra
import Output.Colors exposing (mkColors)
import Output.OutputUtil exposing (fragmentUrl)


type alias Model =
    { labels : Labels
    , teamQuery : TeamQuery
    , quizRatings : QuizRatings
    , quizInfo : QuizInfo
    , status : Status
    }


updateLabels : Model -> Labels -> Model
updateLabels model labels =
    updateLabelsSet model.status True
        |> updateStatus model
        |> (\m -> { m | labels = labels })


updateQuizInfo : Model -> QuizInfo -> Model
updateQuizInfo model quizInfo =
    updateQuizInfoSet model.status True
        |> updateStatus model
        |> (\m -> { m | quizInfo = quizInfo })


updateQuizRatings : Model -> QuizRatings -> Model
updateQuizRatings model quizRatings =
    updateQuizRatingsSet model.status True
        |> updateStatus model
        |> (\m -> { m | quizRatings = quizRatings })


updateStatus : Model -> Status -> Model
updateStatus model status =
    { model | status = status }


type alias Status =
    { labelsSet : Bool
    , quizInfoSet : Bool
    , quizRatingsSet : Bool
    }


isFinished : Status -> Bool
isFinished s =
    List.all identity [ s.labelsSet, s.quizInfoSet, s.quizRatingsSet ]


updateLabelsSet : Status -> Bool -> Status
updateLabelsSet status b =
    { status | labelsSet = b }


updateQuizInfoSet : Status -> Bool -> Status
updateQuizInfoSet status b =
    { status | quizInfoSet = b }


updateQuizRatingsSet : Status -> Bool -> Status
updateQuizRatingsSet status b =
    { status | quizRatingsSet = b }


teamTableInfoOfModel : Model -> TeamTableInfo
teamTableInfoOfModel model =
    teamTableInfoWith model.labels.teamLabel model.teamQuery model.quizRatings


teamTableInfoWith : String -> TeamQuery -> QuizRatings -> TeamTableInfo
teamTableInfoWith wordForTeam teamQuery quizRatings =
    { teamTableInfoTeamName =
        quizRatings.header
            |> List.Extra.find (\teamInfo -> teamInfo.teamInfoNumber == teamQuery.teamQueryTeamNumber)
            |> Util.foldMaybe
                (String.join " " [ wordForTeam, String.fromInt teamQuery.teamQueryTeamNumber ])
                .teamInfoName
    , teamTableInfoNumberOfTeams =
        quizRatings.header
            |> List.filter (.teamInfoActivity >> QuizValues.isActive)
            |> List.length
    , teamTableInfoTeamNumber = teamQuery.teamQueryTeamNumber
    , teamTable =
        List.map (uncurry (roundRatingToTeamLine teamQuery.teamQueryTeamNumber)) quizRatings.ratings
    }


roundRatingToTeamLine : TeamNumber -> RoundNumber -> RoundRating -> TeamLine
roundRatingToTeamLine teamNumber roundNumber roundRating =
    { roundNumber = roundNumber
    , reachedPoints =
        roundRating.points
            |> List.Extra.find (\teamRating -> teamRating.teamNumber == teamNumber)
            |> Util.foldMaybe 0 .rating
    , maximumPoints =
        roundRating.points
            |> List.Extra.maximumBy .rating
            |> Util.foldMaybe 0 .rating
    , reachablePoints = roundRating.reachableInRound
    }


init : Maybe Labels -> Maybe QuizInfo -> TeamQuery -> ( Model, Cmd Msg )
init mLabels mQuizInfo teamQuery =
    ( { labels = Maybe.withDefault QuizValues.defaultLabels mLabels
      , teamQuery = teamQuery
      , quizRatings = QuizRatings.default
      , quizInfo = Maybe.withDefault QuizValues.defaultQuizInfo mQuizInfo
      , status =
            { quizRatingsSet = False
            , quizInfoSet = Util.isDefined mQuizInfo
            , labelsSet = Util.isDefined mLabels
            }
      }
    , Cmd.batch
        [ getQuizRatings teamQuery.teamQueryQuizId
        , useOrFetchWith (getLabelsWith GotLabels) mLabels teamQuery.teamQueryQuizId
        , useOrFetchWith (getQuizInfoWith GotQuizInfo) mQuizInfo teamQuery.teamQueryQuizId
        ]
    )


type Msg
    = GotLabels (ErrorOr Labels)
    | GotQuizInfo (ErrorOr QuizInfo)
    | GotQuizRatings (ErrorOr QuizRatings)


view : Model -> Html Msg
view model =
    if not (isFinished model.status) then
        div [] [ loadingSymbol ]

    else
        let
            teamTableInfo =
                teamTableInfoOfModel model

            colors =
                mkColors teamTableInfo.teamTableInfoNumberOfTeams

            colorSetting =
                model.quizRatings.header
                    |> List.filter (.teamInfoActivity >> QuizValues.isActive)
                    |> List.Extra.findIndex (\ti -> ti.teamInfoNumber == teamTableInfo.teamTableInfoTeamNumber)
                    |> Maybe.andThen (\index -> List.Extra.getAt index colors)
                    |> Maybe.map (\c -> [ style "color" (Color.Convert.colorToCssRgba c) ])
                    |> Maybe.withDefault []
        in
        div [ id "tableView" ]
            [ div [ id "ownPoints" ]
                [ h1 colorSetting
                    [ label [ for "ownPointsLabel" ]
                        [ text teamTableInfo.teamTableInfoTeamName
                        , text ": "
                        , text (showStanding (standing teamTableInfo.teamTable))
                        ]
                    ]
                ]
            , div [ id "pointsTable" ]
                [ table
                    []
                    (tr [ class "tableHeader" ]
                        [ th [] [ label [ for "roundLabel" ] [ text model.labels.roundLabel ] ]
                        , th [] [ label [ for "ownPointsLabel" ] [ text model.labels.ownPointsLabel ] ]
                        , th [] [ label [ for "maxReachedLabel" ] [ text model.labels.maxReachedLabel ] ]
                        , th [] [ label [ for "maxReachableLabel" ] [ text model.labels.maxReachableLabel ] ]
                        , th [] [ label [ for "placeInRound" ] [ text model.labels.placeInRoundLabel ] ]
                        , th [] [ label [ for "placeAfterRound" ] [ text model.labels.placeAfterRoundLabel ] ]
                        ]
                        :: List.map3 mkHTMLLine
                            teamTableInfo.teamTable
                            (findPositions model.quizRatings.ratings teamTableInfo.teamTableInfoTeamNumber)
                            (findPositions (cumulative model.quizRatings).ratings teamTableInfo.teamTableInfoTeamNumber)
                    )
                ]
            , div [ id "quizRatings" ]
                [ linkButton
                    (fragmentUrl [ quizIdParam, String.fromInt model.teamQuery.teamQueryQuizId ])
                    [ class "quizRatingsButton", value model.labels.backToChartView ]
                    []
                ]
            ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        newModel =
            case msg of
                GotLabels labelsCandidate ->
                    Util.foldResult model (updateLabels model) labelsCandidate

                GotQuizInfo quizInfoCandidate ->
                    Util.foldResult model (updateQuizInfo model) quizInfoCandidate

                GotQuizRatings quizRatingsCandidate ->
                    Util.foldResult model (updateQuizRatings model) quizRatingsCandidate
    in
    ( newModel, Cmd.none )


standing : TeamTable -> ( Float, Float )
standing =
    List.foldr (\tl ( reached, reachable ) -> ( tl.reachedPoints + reached, tl.reachablePoints + reachable )) ( 0, 0 )


showStanding : ( Float, Float ) -> String
showStanding ( reached, reachable ) =
    String.join "/" [ String.fromFloat reached, String.fromFloat reachable ]


type alias PositionInRound =
    { roundNumber : RoundNumber
    , position : Int
    }


type alias Positioned =
    { position : Int
    , teams : List TeamNumber
    }


sortPoints : List TeamRating -> List Positioned
sortPoints teamRatings =
    teamRatings
        |> List.sortBy .rating
        |> Util.groupBy (\x y -> x.rating == y.rating)
        |> List.reverse
        |> List.indexedMap (\pos trs -> { position = 1 + pos, teams = List.map .teamNumber trs })


findPositions : Ratings -> TeamNumber -> List PositionInRound
findPositions ratings teamNumber =
    ratings
        |> List.map
            (\( roundNumber, roundRating ) ->
                { roundNumber = roundNumber
                , position =
                    roundRating.points
                        |> sortPoints
                        |> List.Extra.find (\positioned -> List.any ((==) teamNumber) positioned.teams)
                        |> Util.foldMaybe 0 .position
                }
            )


mkHTMLLine : TeamLine -> PositionInRound -> PositionInRound -> Html Msg
mkHTMLLine teamLine perRound cumulative =
    tr []
        [ mkCell (String.fromInt teamLine.roundNumber)
        , mkCell (String.fromFloat teamLine.reachedPoints)
        , mkCell (String.fromFloat teamLine.maximumPoints)
        , mkCell (String.fromFloat teamLine.reachablePoints)
        , mkCell (String.fromInt perRound.position)
        , mkCell (String.fromInt cumulative.position)
        ]


mkCell : String -> Html Msg
mkCell str =
    td [] [ text str ]


getQuizRatings : DbQuizId -> Cmd Msg
getQuizRatings =
    getMsg getQuizRatingsApi GotQuizRatings jsonDecQuizRatings
