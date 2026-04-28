module Generated.Api.Json exposing
    ( encodeAddTeamsCommand, encodeChangeSettingsCommand, encodeCorrectScoreCommand, encodeDay
    , encodeLoginRequest, encodeLoginResponse, encodeQuizActive, encodeQuizIdentifier, encodeQuizMetaData
    , encodeQuizSettings, encodeQuizSummary, encodeRecordRoundScoresCommand, encodeRenameTeamCommand
    , encodeRound, encodeScoreBoard, encodeSetTeamActiveCommand, encodeTeam
    , decodeAddTeamsCommand, decodeChangeSettingsCommand, decodeCorrectScoreCommand, decodeDay
    , decodeLoginRequest, decodeLoginResponse, decodeQuizActive, decodeQuizIdentifier, decodeQuizMetaData
    , decodeQuizSettings, decodeQuizSummary, decodeRecordRoundScoresCommand, decodeRenameTeamCommand
    , decodeRound, decodeScoreBoard, decodeSetTeamActiveCommand, decodeTeam
    )

{-|


## Encoders

@docs encodeAddTeamsCommand, encodeChangeSettingsCommand, encodeCorrectScoreCommand, encodeDay
@docs encodeLoginRequest, encodeLoginResponse, encodeQuizActive, encodeQuizIdentifier, encodeQuizMetaData
@docs encodeQuizSettings, encodeQuizSummary, encodeRecordRoundScoresCommand, encodeRenameTeamCommand
@docs encodeRound, encodeScoreBoard, encodeSetTeamActiveCommand, encodeTeam


## Decoders

@docs decodeAddTeamsCommand, decodeChangeSettingsCommand, decodeCorrectScoreCommand, decodeDay
@docs decodeLoginRequest, decodeLoginResponse, decodeQuizActive, decodeQuizIdentifier, decodeQuizMetaData
@docs decodeQuizSettings, decodeQuizSummary, decodeRecordRoundScoresCommand, decodeRenameTeamCommand
@docs decodeRound, decodeScoreBoard, decodeSetTeamActiveCommand, decodeTeam

-}

import Generated.Api.Types
import Date
import Json.Decode
import Json.Encode
import Generated.OpenApi.Common


decodeTeam : Json.Decode.Decoder Generated.Api.Types.Team
decodeTeam =
    Json.Decode.succeed
        (\active name number ->
            { active = active, name = name, number = number }
        )
        |> Generated.OpenApi.Common.jsonDecodeAndMap
            (Json.Decode.field "active" Json.Decode.bool)
        |> Generated.OpenApi.Common.jsonDecodeAndMap
            (Json.Decode.field "name" Json.Decode.string)
        |> Generated.OpenApi.Common.jsonDecodeAndMap
            (Json.Decode.field
                "number"
                Json.Decode.int
            )


encodeTeam : Generated.Api.Types.Team -> Json.Encode.Value
encodeTeam rec =
    Json.Encode.object
        [ ( "active", Json.Encode.bool rec.active )
        , ( "name", Json.Encode.string rec.name )
        , ( "number", Json.Encode.int rec.number )
        ]


decodeSetTeamActiveCommand : Json.Decode.Decoder Generated.Api.Types.SetTeamActiveCommand
decodeSetTeamActiveCommand =
    Json.Decode.succeed
        (\active teamNumber -> { active = active, teamNumber = teamNumber })
        |> Generated.OpenApi.Common.jsonDecodeAndMap
            (Json.Decode.field "active" Json.Decode.bool)
        |> Generated.OpenApi.Common.jsonDecodeAndMap
            (Json.Decode.field
                "teamNumber"
                Json.Decode.int
            )


encodeSetTeamActiveCommand : Generated.Api.Types.SetTeamActiveCommand -> Json.Encode.Value
encodeSetTeamActiveCommand rec =
    Json.Encode.object
        [ ( "active", Json.Encode.bool rec.active )
        , ( "teamNumber", Json.Encode.int rec.teamNumber )
        ]


decodeScoreBoard : Json.Decode.Decoder Generated.Api.Types.ScoreBoard
decodeScoreBoard =
    Json.Decode.succeed
        (\scores teams -> { scores = scores, teams = teams })
        |> Generated.OpenApi.Common.jsonDecodeAndMap
            (Json.Decode.field
                "scores"
                (Json.Decode.list Json.Decode.value)
            )
        |> Generated.OpenApi.Common.jsonDecodeAndMap
            (Json.Decode.field
                "teams"
                (Json.Decode.list decodeTeam)
            )


encodeScoreBoard : Generated.Api.Types.ScoreBoard -> Json.Encode.Value
encodeScoreBoard rec =
    Json.Encode.object
        [ ( "scores", Json.Encode.list Basics.identity rec.scores )
        , ( "teams", Json.Encode.list encodeTeam rec.teams )
        ]


decodeRound : Json.Decode.Decoder Generated.Api.Types.Round
decodeRound =
    Json.Decode.succeed
        (\displayMaxPoints number numberOfQuestions ->
            { displayMaxPoints = displayMaxPoints
            , number = number
            , numberOfQuestions = numberOfQuestions
            }
        )
        |> Generated.OpenApi.Common.jsonDecodeAndMap
            (Json.Decode.field "displayMaxPoints" Json.Decode.float)
        |> Generated.OpenApi.Common.jsonDecodeAndMap
            (Json.Decode.field "number" Json.Decode.int)
        |> Generated.OpenApi.Common.jsonDecodeAndMap
            (Json.Decode.field
                "numberOfQuestions"
                Json.Decode.int
            )


encodeRound : Generated.Api.Types.Round -> Json.Encode.Value
encodeRound rec =
    Json.Encode.object
        [ ( "displayMaxPoints", Json.Encode.float rec.displayMaxPoints )
        , ( "number", Json.Encode.int rec.number )
        , ( "numberOfQuestions", Json.Encode.int rec.numberOfQuestions )
        ]


decodeRenameTeamCommand : Json.Decode.Decoder Generated.Api.Types.RenameTeamCommand
decodeRenameTeamCommand =
    Json.Decode.succeed
        (\newName teamNumber -> { newName = newName, teamNumber = teamNumber })
        |> Generated.OpenApi.Common.jsonDecodeAndMap
            (Json.Decode.field "newName" Json.Decode.string)
        |> Generated.OpenApi.Common.jsonDecodeAndMap
            (Json.Decode.field
                "teamNumber"
                Json.Decode.int
            )


encodeRenameTeamCommand : Generated.Api.Types.RenameTeamCommand -> Json.Encode.Value
encodeRenameTeamCommand rec =
    Json.Encode.object
        [ ( "newName", Json.Encode.string rec.newName )
        , ( "teamNumber", Json.Encode.int rec.teamNumber )
        ]


decodeRecordRoundScoresCommand : Json.Decode.Decoder Generated.Api.Types.RecordRoundScoresCommand
decodeRecordRoundScoresCommand =
    Json.Decode.succeed
        (\roundNumber scores -> { roundNumber = roundNumber, scores = scores })
        |> Generated.OpenApi.Common.jsonDecodeAndMap
            (Json.Decode.field "roundNumber" Json.Decode.int)
        |> Generated.OpenApi.Common.jsonDecodeAndMap
            (Json.Decode.field
                "scores"
                (Json.Decode.list Json.Decode.value)
            )


encodeRecordRoundScoresCommand : Generated.Api.Types.RecordRoundScoresCommand -> Json.Encode.Value
encodeRecordRoundScoresCommand rec =
    Json.Encode.object
        [ ( "roundNumber", Json.Encode.int rec.roundNumber )
        , ( "scores", Json.Encode.list Basics.identity rec.scores )
        ]


decodeQuizActive : Json.Decode.Decoder Generated.Api.Types.QuizActive
decodeQuizActive =
    Json.Decode.succeed
        (\identifier quizId rounds scoreBoard ->
            { identifier = identifier
            , quizId = quizId
            , rounds = rounds
            , scoreBoard = scoreBoard
            }
        )
        |> Generated.OpenApi.Common.jsonDecodeAndMap
            (Json.Decode.field "identifier" decodeQuizIdentifier)
        |> Generated.OpenApi.Common.jsonDecodeAndMap
            (Json.Decode.field "quizId" Json.Decode.int)
        |> Generated.OpenApi.Common.jsonDecodeAndMap
            (Json.Decode.field
                "rounds"
                (Json.Decode.list
                    decodeRound
                )
            )
        |> Generated.OpenApi.Common.jsonDecodeAndMap
            (Json.Decode.field
                "scoreBoard"
                decodeScoreBoard
            )


encodeQuizActive : Generated.Api.Types.QuizActive -> Json.Encode.Value
encodeQuizActive rec =
    Json.Encode.object
        [ ( "identifier", encodeQuizIdentifier rec.identifier )
        , ( "quizId", Json.Encode.int rec.quizId )
        , ( "rounds", Json.Encode.list encodeRound rec.rounds )
        , ( "scoreBoard", encodeScoreBoard rec.scoreBoard )
        ]


decodeQuizSummary : Json.Decode.Decoder Generated.Api.Types.QuizSummary
decodeQuizSummary =
    Json.Decode.succeed
        (\active identifier quizId ->
            { active = active, identifier = identifier, quizId = quizId }
        )
        |> Generated.OpenApi.Common.jsonDecodeAndMap
            (Json.Decode.field "active" Json.Decode.bool)
        |> Generated.OpenApi.Common.jsonDecodeAndMap
            (Json.Decode.field
                "identifier"
                decodeQuizIdentifier
            )
        |> Generated.OpenApi.Common.jsonDecodeAndMap
            (Json.Decode.field
                "quizId"
                Json.Decode.int
            )


encodeQuizSummary : Generated.Api.Types.QuizSummary -> Json.Encode.Value
encodeQuizSummary rec =
    Json.Encode.object
        [ ( "active", Json.Encode.bool rec.active )
        , ( "identifier", encodeQuizIdentifier rec.identifier )
        , ( "quizId", Json.Encode.int rec.quizId )
        ]


decodeQuizSettings : Json.Decode.Decoder Generated.Api.Types.QuizSettings
decodeQuizSettings =
    Json.Decode.succeed
        (\numberOfTeams questionsPerRound ->
            { numberOfTeams = numberOfTeams
            , questionsPerRound = questionsPerRound
            }
        )
        |> Generated.OpenApi.Common.jsonDecodeAndMap
            (Json.Decode.field "numberOfTeams" Json.Decode.int)
        |> Generated.OpenApi.Common.jsonDecodeAndMap
            (Json.Decode.field
                "questionsPerRound"
                (Json.Decode.list Json.Decode.int)
            )


encodeQuizSettings : Generated.Api.Types.QuizSettings -> Json.Encode.Value
encodeQuizSettings rec =
    Json.Encode.object
        [ ( "numberOfTeams", Json.Encode.int rec.numberOfTeams )
        , ( "questionsPerRound"
          , Json.Encode.list Json.Encode.int rec.questionsPerRound
          )
        ]


decodeQuizMetaData : Json.Decode.Decoder Generated.Api.Types.QuizMetaData
decodeQuizMetaData =
    Json.Decode.succeed
        (\identifier settings ->
            { identifier = identifier, settings = settings }
        )
        |> Generated.OpenApi.Common.jsonDecodeAndMap
            (Json.Decode.field "identifier" decodeQuizIdentifier)
        |> Generated.OpenApi.Common.jsonDecodeAndMap
            (Json.Decode.field
                "settings"
                decodeQuizSettings
            )


encodeQuizMetaData : Generated.Api.Types.QuizMetaData -> Json.Encode.Value
encodeQuizMetaData rec =
    Json.Encode.object
        [ ( "identifier", encodeQuizIdentifier rec.identifier )
        , ( "settings", encodeQuizSettings rec.settings )
        ]


decodeQuizIdentifier : Json.Decode.Decoder Generated.Api.Types.QuizIdentifier
decodeQuizIdentifier =
    Json.Decode.succeed
        (\date name place -> { date = date, name = name, place = place })
        |> Generated.OpenApi.Common.jsonDecodeAndMap
            (Json.Decode.field "date" decodeDay)
        |> Generated.OpenApi.Common.jsonDecodeAndMap
            (Json.Decode.field "name" Json.Decode.string)
        |> Generated.OpenApi.Common.jsonDecodeAndMap
            (Json.Decode.field
                "place"
                Json.Decode.string
            )


encodeQuizIdentifier : Generated.Api.Types.QuizIdentifier -> Json.Encode.Value
encodeQuizIdentifier rec =
    Json.Encode.object
        [ ( "date", encodeDay rec.date )
        , ( "name", Json.Encode.string rec.name )
        , ( "place", Json.Encode.string rec.place )
        ]


decodeLoginResponse : Json.Decode.Decoder Generated.Api.Types.LoginResponse
decodeLoginResponse =
    Json.Decode.succeed
        (\token -> { token = token })
        |> Generated.OpenApi.Common.jsonDecodeAndMap
            (Json.Decode.field
                "token"
                Json.Decode.string
            )


encodeLoginResponse : Generated.Api.Types.LoginResponse -> Json.Encode.Value
encodeLoginResponse rec =
    Json.Encode.object [ ( "token", Json.Encode.string rec.token ) ]


decodeLoginRequest : Json.Decode.Decoder Generated.Api.Types.LoginRequest
decodeLoginRequest =
    Json.Decode.succeed
        (\password username -> { password = password, username = username })
        |> Generated.OpenApi.Common.jsonDecodeAndMap
            (Json.Decode.field "password" Json.Decode.string)
        |> Generated.OpenApi.Common.jsonDecodeAndMap
            (Json.Decode.field
                "username"
                Json.Decode.string
            )


encodeLoginRequest : Generated.Api.Types.LoginRequest -> Json.Encode.Value
encodeLoginRequest rec =
    Json.Encode.object
        [ ( "password", Json.Encode.string rec.password )
        , ( "username", Json.Encode.string rec.username )
        ]


decodeDay : Json.Decode.Decoder Generated.Api.Types.Day
decodeDay =
    Json.Decode.andThen
        (\andThenUnpack ->
            case Date.fromIsoString andThenUnpack of
                Result.Ok value ->
                    Json.Decode.succeed value

                Result.Err error ->
                    Json.Decode.fail error
        )
        Json.Decode.string


encodeDay : Generated.Api.Types.Day -> Json.Encode.Value
encodeDay rec =
    Json.Encode.string (Date.toIsoString rec)


decodeCorrectScoreCommand : Json.Decode.Decoder Generated.Api.Types.CorrectScoreCommand
decodeCorrectScoreCommand =
    Json.Decode.succeed
        (\points roundNumber teamNumber ->
            { points = points
            , roundNumber = roundNumber
            , teamNumber = teamNumber
            }
        )
        |> Generated.OpenApi.Common.jsonDecodeAndMap
            (Json.Decode.field "points" Json.Decode.float)
        |> Generated.OpenApi.Common.jsonDecodeAndMap
            (Json.Decode.field
                "roundNumber"
                Json.Decode.int
            )
        |> Generated.OpenApi.Common.jsonDecodeAndMap
            (Json.Decode.field
                "teamNumber"
                Json.Decode.int
            )


encodeCorrectScoreCommand : Generated.Api.Types.CorrectScoreCommand -> Json.Encode.Value
encodeCorrectScoreCommand rec =
    Json.Encode.object
        [ ( "points", Json.Encode.float rec.points )
        , ( "roundNumber", Json.Encode.int rec.roundNumber )
        , ( "teamNumber", Json.Encode.int rec.teamNumber )
        ]


decodeChangeSettingsCommand : Json.Decode.Decoder Generated.Api.Types.ChangeSettingsCommand
decodeChangeSettingsCommand =
    Json.Decode.succeed
        (\newIdentifier -> { newIdentifier = newIdentifier })
        |> Generated.OpenApi.Common.jsonDecodeAndMap
            (Json.Decode.field "newIdentifier" decodeQuizIdentifier)


encodeChangeSettingsCommand : Generated.Api.Types.ChangeSettingsCommand -> Json.Encode.Value
encodeChangeSettingsCommand rec =
    Json.Encode.object
        [ ( "newIdentifier", encodeQuizIdentifier rec.newIdentifier ) ]


decodeAddTeamsCommand : Json.Decode.Decoder Generated.Api.Types.AddTeamsCommand
decodeAddTeamsCommand =
    Json.Decode.succeed
        (\additionalTeams -> { additionalTeams = additionalTeams })
        |> Generated.OpenApi.Common.jsonDecodeAndMap
            (Json.Decode.field "additionalTeams" Json.Decode.int)


encodeAddTeamsCommand : Generated.Api.Types.AddTeamsCommand -> Json.Encode.Value
encodeAddTeamsCommand rec =
    Json.Encode.object
        [ ( "additionalTeams", Json.Encode.int rec.additionalTeams ) ]
