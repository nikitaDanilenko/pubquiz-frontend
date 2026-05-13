module Util.Team exposing (activeTeams, teamName)

import Api.Types exposing (Team)


activeTeams : List Team -> List Team
activeTeams =
    List.filter .active >> List.sortBy .number


teamName : Team -> String
teamName team =
    if String.isEmpty team.name then
        String.concat [ "Team ", String.fromInt team.number ]

    else
        team.name
