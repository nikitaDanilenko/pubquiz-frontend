module Output.Table exposing (..)

import Color.Convert
import Common.Constants exposing (teamQueryParam, teamTableApi)
import Common.Types exposing (DbQuizId, Labels, QuizInfo, QuizRatings, TeamLine, TeamQuery, TeamTable, TeamTableInfo, jsonDecTeamTableInfo, jsonEncTeamQuery)
import Common.Util as Util exposing (getMsgWith)
import Html exposing (Html, button, div, h1, table, td, text, th, tr)
import Html.Attributes exposing (class, id, style)
import Html.Events exposing (onClick)
import Input.Model as Input exposing (ErrorOr)
import List.Extra
import Output.Colors exposing (mkColors)


type alias Model =
    { labels : Labels
    , teamQuery : TeamQuery
    , teamTableInfo : TeamTableInfo
    , quizInfo : QuizInfo
    }


init : TeamQuery -> ( Model, Cmd Msg )
init teamQuery =
    ( { labels = Input.defaultLabels
      , teamQuery =
            { teamQueryQuizId = -1
            , teamQueryTeamNumber = 0
            , teamQueryTeamCode = ""
            }
      , teamTableInfo =
            { teamTable = []
            , teamTableInfoTeamName = ""
            , teamTableInfoNumberOfTeams = 0
            , teamTableInfoTeamNumber = 0
            }
      , quizInfo = Input.defaultQuizInfo
      }
    , getTeamTable teamQuery
    )


type Msg
    = GetQuizRatings DbQuizId
    | GotTeamTableInfo (ErrorOr TeamTableInfo)


view : Model -> Html Msg
view model =
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
                [ text model.teamTableInfo.teamTableInfoTeamName
                , text ": "
                , text (showStanding (standing model.teamTableInfo.teamTable))
                ]
            ]
        , div [ id "pointsTable" ]
            [ table
                []
                (tr []
                    [ th [] [ text model.labels.roundLabel ]
                    , th [] [ text model.labels.ownPointsLabel ]
                    , th [] [ text model.labels.maxReachedLabel ]
                    , th [] [ text model.labels.maxReachableLabel ]
                    ]
                    :: List.map mkHTMLLine model.teamTableInfo.teamTable
                )
            ]
        , div [ id "quizRatings" ]
            [ button [ class "quizRatingsButton", onClick (GetQuizRatings model.quizInfo.quizId) ] [ text model.labels.backToChartView ] ]
        ]


standing : TeamTable -> ( Float, Float )
standing =
    List.foldr (\tl ( reached, reachable ) -> ( tl.reachedPoints + reached, tl.reachablePoints + reachable )) ( 0, 0 )


showStanding : ( Float, Float ) -> String
showStanding ( reached, reachable ) =
    String.join "/" [ String.fromFloat reached, String.fromFloat reachable ]


mkHTMLLine : TeamLine -> Html Msg
mkHTMLLine ti =
    tr []
        [ td [] [ text (String.fromInt ti.roundNumber) ]
        , td [] [ text (String.fromFloat ti.reachedPoints) ]
        , td [] [ text (String.fromFloat ti.maximumPoints) ]
        , td [] [ text (String.fromFloat ti.reachablePoints) ]
        ]


getTeamTable : TeamQuery -> Cmd Msg
getTeamTable =
    getMsgWith jsonEncTeamQuery teamQueryParam teamTableApi GotTeamTableInfo jsonDecTeamTableInfo
