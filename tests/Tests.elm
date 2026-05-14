module Tests exposing (..)

import Api.Types exposing (Team)
import Expect
import Fuzz exposing (bool, int, string)
import Test exposing (..)
import Util.Team exposing (activeTeams, teamName)


mkTeam : Int -> String -> Bool -> Team
mkTeam number name active =
    { number = number, name = name, active = active }


teamNameTests : Test
teamNameTests =
    describe "teamName"
        [ test "uses name when non-empty" <|
            \_ ->
                Expect.equal "Quizzards" (teamName (mkTeam 1 "Quizzards" True))
        , test "falls back to Team N when name is empty" <|
            \_ ->
                Expect.equal "Team 3" (teamName (mkTeam 3 "" True))
        , fuzz int "fallback contains team number" <|
            \n ->
                let
                    result =
                        teamName (mkTeam n "" True)
                in
                Expect.equal True (String.startsWith "Team " result)
        , fuzz string "non-empty name is returned unchanged" <|
            \name ->
                let
                    nonEmpty =
                        "x" ++ name
                in
                Expect.equal nonEmpty (teamName (mkTeam 1 nonEmpty True))
        ]


activeTeamsTests : Test
activeTeamsTests =
    describe "activeTeams"
        [ test "filters out inactive teams" <|
            \_ ->
                let
                    teams =
                        [ mkTeam 1 "A" True, mkTeam 2 "B" False, mkTeam 3 "C" True ]
                in
                Expect.equal [ 1, 3 ] (List.map .number (activeTeams teams))
        , test "sorts by number" <|
            \_ ->
                let
                    teams =
                        [ mkTeam 3 "C" True, mkTeam 1 "A" True, mkTeam 2 "B" True ]
                in
                Expect.equal [ 1, 2, 3 ] (List.map .number (activeTeams teams))
        , fuzz bool "inactive team never appears in result" <|
            \_ ->
                let
                    team =
                        mkTeam 42 "Solo" False

                    result =
                        activeTeams [ team ]
                in
                Expect.equal [] result
        , fuzz (Fuzz.list (Fuzz.map3 mkTeam int string bool)) "result contains only active teams" <|
            \teams ->
                let
                    result =
                        activeTeams teams
                in
                Expect.equal True (List.all .active result)
        , fuzz (Fuzz.list (Fuzz.map3 mkTeam int string bool)) "result is sorted by number" <|
            \teams ->
                let
                    numbers =
                        List.map .number (activeTeams teams)
                in
                Expect.equal numbers (List.sort numbers)
        ]


all : Test
all =
    describe "Util.Team"
        [ teamNameTests
        , activeTeamsTests
        ]