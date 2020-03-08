module Output.Table exposing (Model, Msg, init, update, view)

import Color.Convert
import Common.ConnectionUtil exposing (getLabelsWith, getQuizInfoWith, linkButton, useOrFetchWith)
import Common.Constants exposing (quizIdParam, teamQueryParam, teamTableApi)
import Common.Types exposing (DbQuizId, Labels, QuizInfo, QuizRatings, TeamLine, TeamQuery, TeamTable, TeamTableInfo, jsonDecTeamTableInfo, jsonEncTeamQuery)
import Common.Util as Util exposing (getMsgWith)
import Html exposing (Html, div, h1, label, table, td, text, th, tr)
import Html.Attributes exposing (class, for, id, style, value)
import Input.Model as Input exposing (ErrorOr)
import List.Extra
import Output.Colors exposing (mkColors)
import Output.OutputUtil exposing (fragmentUrl)


type alias Model =
    { labels : Labels
    , teamQuery : TeamQuery
    , teamTableInfo : TeamTableInfo
    , quizInfo : QuizInfo
    , status : Status
    }


updateLabels : Model -> Labels -> Model
updateLabels model labels =
    updateLabelsSet model.status True
        |> updateStatus model
        |> (\m -> { m | labels = labels })


updateTeamTableInfo : Model -> TeamTableInfo -> Model
updateTeamTableInfo model teamTableInfo =
    updateTeamTableInfoSet model.status True
        |> updateStatus model
        |> (\m -> { m | teamTableInfo = teamTableInfo })


updateQuizInfo : Model -> QuizInfo -> Model
updateQuizInfo model quizInfo =
    updateQuizInfoSet model.status True
        |> updateStatus model
        |> (\m -> { m | quizInfo = quizInfo })


updateStatus : Model -> Status -> Model
updateStatus model status =
    { model | status = status }


type alias Status =
    { labelsSet : Bool
    , quizInfoSet : Bool
    , teamTableInfoSet : Bool
    }


isFinished : Status -> Bool
isFinished s =
    List.all identity [ s.labelsSet, s.quizInfoSet, s.teamTableInfoSet ]


updateLabelsSet : Status -> Bool -> Status
updateLabelsSet status b =
    { status | labelsSet = b }


updateQuizInfoSet : Status -> Bool -> Status
updateQuizInfoSet status b =
    { status | quizInfoSet = b }


updateTeamTableInfoSet : Status -> Bool -> Status
updateTeamTableInfoSet status b =
    { status | teamTableInfoSet = b }


init : Maybe Labels -> Maybe QuizInfo -> TeamQuery -> ( Model, Cmd Msg )
init mLabels mQuizInfo teamQuery =
    ( { labels = Maybe.withDefault Input.defaultLabels mLabels
      , teamQuery = teamQuery
      , teamTableInfo =
            { teamTable = []
            , teamTableInfoTeamName = ""
            , teamTableInfoNumberOfTeams = 0
            , teamTableInfoTeamNumber = 0
            }
      , quizInfo = Maybe.withDefault Input.defaultQuizInfo mQuizInfo
      , status =
            { teamTableInfoSet = False
            , quizInfoSet = Util.isDefined mQuizInfo
            , labelsSet = Util.isDefined mLabels
            }
      }
    , Cmd.batch
        [ getTeamTableInfo teamQuery
        , useOrFetchWith (getLabelsWith GotLabels) mLabels teamQuery.teamQueryQuizId
        , useOrFetchWith (getQuizInfoWith GotQuizInfo) mQuizInfo teamQuery.teamQueryQuizId
        ]
    )


type Msg
    = GotTeamTableInfo (ErrorOr TeamTableInfo)
    | GotLabels (ErrorOr Labels)
    | GotQuizInfo (ErrorOr QuizInfo)


view : Model -> Html Msg
view model =
    if not (isFinished model.status) then
        -- todo add loading symbol
        div [] []

    else
        let
            colors =
                mkColors model.teamTableInfo.teamTableInfoNumberOfTeams

            colorSetting =
                Util.foldMaybe []
                    (\c -> [ style "color" (Color.Convert.colorToCssRgba c) ])
                    (List.Extra.getAt model.teamTableInfo.teamTableInfoTeamNumber colors)
        in
        div [ id "tableView" ]
            [ div [ id "ownPoints" ]
                [ h1 colorSetting
                    [ label [ for "ownPointsLabel" ]
                        [ text model.teamTableInfo.teamTableInfoTeamName
                        , text ": "
                        , text (showStanding (standing model.teamTableInfo.teamTable))
                        ]
                    ]
                ]
            , div [ id "pointsTable" ]
                [ table
                    []
                    (tr []
                        [ th [] [ label [for "roundLabel"] [ text model.labels.roundLabel] ]
                        , th [] [ label [for "ownPointsLabel"] [ text model.labels.ownPointsLabel] ]
                        , th [] [ label [for "maxReachedLabel"] [ text model.labels.maxReachedLabel] ]
                        , th [] [ label [for "maxReachableLabelt"] [ text model.labels.maxReachableLabel] ]
                        ]
                        :: List.map mkHTMLLine model.teamTableInfo.teamTable
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
                GotTeamTableInfo teamTableInfoCandidate ->
                    Util.foldResult model (updateTeamTableInfo model) teamTableInfoCandidate

                GotLabels labelsCandidate ->
                    Util.foldResult model (updateLabels model) labelsCandidate

                GotQuizInfo quizInfoCandidate ->
                    Util.foldResult model (updateQuizInfo model) quizInfoCandidate
    in
    ( newModel, Cmd.none )


standing : TeamTable -> ( Float, Float )
standing =
    List.foldr (\tl ( reached, reachable ) -> ( tl.reachedPoints + reached, tl.reachablePoints + reachable )) ( 0, 0 )


showStanding : ( Float, Float ) -> String
showStanding ( reached, reachable ) =
    String.join "/" [ String.fromFloat reached, String.fromFloat reachable ]


mkHTMLLine : TeamLine -> Html Msg
mkHTMLLine ti =
    tr []
        [ mkCell (String.fromInt ti.roundNumber)
        , mkCell (String.fromFloat ti.reachedPoints)
        , mkCell (String.fromFloat ti.maximumPoints)
        , mkCell (String.fromFloat ti.reachablePoints)
        ]


mkCell : String -> Html Msg
mkCell str =
    td [] [ text str ]


getTeamTableInfo : TeamQuery -> Cmd Msg
getTeamTableInfo =
    getMsgWith jsonEncTeamQuery teamQueryParam teamTableApi GotTeamTableInfo jsonDecTeamTableInfo
