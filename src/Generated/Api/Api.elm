module Api.Api exposing
    ( backoffice, backofficeLogin, backofficeLoginTask, backofficeQuizIdAddTeams, backofficeQuizIdAddTeamsTask
    , backofficeQuizIdChangeSettings, backofficeQuizIdChangeSettingsTask, backofficeQuizIdCorrectScore
    , backofficeQuizIdCorrectScoreTask, backofficeQuizIdLock, backofficeQuizIdLockTask
    , backofficeQuizIdRecordRoundScores, backofficeQuizIdRecordRoundScoresTask, backofficeQuizIdRenameTeam
    , backofficeQuizIdRenameTeamTask, backofficeQuizIdSetTeamActive, backofficeQuizIdSetTeamActiveTask
    , backofficeQuizIdUnlock, backofficeQuizIdUnlockTask, backofficeTask, public, publicQuizId, publicQuizIdTask
    , publicTask
    )

{-|


## Operations

@docs backoffice, backofficeLogin, backofficeLoginTask, backofficeQuizIdAddTeams, backofficeQuizIdAddTeamsTask
@docs backofficeQuizIdChangeSettings, backofficeQuizIdChangeSettingsTask, backofficeQuizIdCorrectScore
@docs backofficeQuizIdCorrectScoreTask, backofficeQuizIdLock, backofficeQuizIdLockTask
@docs backofficeQuizIdRecordRoundScores, backofficeQuizIdRecordRoundScoresTask, backofficeQuizIdRenameTeam
@docs backofficeQuizIdRenameTeamTask, backofficeQuizIdSetTeamActive, backofficeQuizIdSetTeamActiveTask
@docs backofficeQuizIdUnlock, backofficeQuizIdUnlockTask, backofficeTask, public, publicQuizId, publicQuizIdTask
@docs publicTask

-}

import Api.Types
import Bytes
import Dict
import Http
import Json.Decode
import OpenApi.Common
import Task
import Url.Builder


backoffice config =
    Http.request
        { url = Url.Builder.absolute [ "backoffice" ] []
        , method = "POST"
        , headers = []
        , expect =
            OpenApi.Common.expectBytesCustom
                config.toMsg
                (Dict.fromList [ ( "400", Json.Decode.succeed () ) ])
        , body = Http.bytesBody "application/json;charset=utf-8" config.body
        , timeout = Nothing
        , tracker = Nothing
        }


backofficeTask :
    { body : Bytes.Bytes }
    -> Task.Task (OpenApi.Common.Error () Bytes.Bytes) Bytes.Bytes
backofficeTask config =
    Http.task
        { url = Url.Builder.absolute [ "backoffice" ] []
        , method = "POST"
        , headers = []
        , resolver =
            OpenApi.Common.bytesResolverCustom
                (Dict.fromList [ ( "400", Json.Decode.succeed () ) ])
        , body = Http.bytesBody "application/json;charset=utf-8" config.body
        , timeout = Nothing
        }


backofficeLogin config =
    Http.request
        { url = Url.Builder.absolute [ "backoffice", "login" ] []
        , method = "POST"
        , headers = []
        , expect =
            OpenApi.Common.expectBytesCustom
                config.toMsg
                (Dict.fromList [ ( "400", Json.Decode.succeed () ) ])
        , body = Http.bytesBody "application/json;charset=utf-8" config.body
        , timeout = Nothing
        , tracker = Nothing
        }


backofficeLoginTask :
    { body : Bytes.Bytes }
    -> Task.Task (OpenApi.Common.Error () Bytes.Bytes) Bytes.Bytes
backofficeLoginTask config =
    Http.task
        { url = Url.Builder.absolute [ "backoffice", "login" ] []
        , method = "POST"
        , headers = []
        , resolver =
            OpenApi.Common.bytesResolverCustom
                (Dict.fromList [ ( "400", Json.Decode.succeed () ) ])
        , body = Http.bytesBody "application/json;charset=utf-8" config.body
        , timeout = Nothing
        }


backofficeQuizIdAddTeams :
    { toMsg :
        Result (OpenApi.Common.Error Api.Types.BackofficeQuizIdAddTeams_Error Bytes.Bytes) Bytes.Bytes
        -> msg
    , body : Bytes.Bytes
    , params : { quizId : Int }
    }
    -> Cmd msg
backofficeQuizIdAddTeams config =
    Http.request
        { url =
            Url.Builder.absolute
                [ "backoffice"
                , String.fromInt config.params.quizId
                , "add-teams"
                ]
                []
        , method = "POST"
        , headers = []
        , expect =
            OpenApi.Common.expectBytesCustom
                config.toMsg
                (Dict.fromList
                    [ ( "400"
                      , Json.Decode.map
                            Api.Types.BackofficeQuizIdAddTeams_400
                            (Json.Decode.succeed ())
                      )
                    , ( "404"
                      , Json.Decode.map
                            Api.Types.BackofficeQuizIdAddTeams_404
                            (Json.Decode.succeed ())
                      )
                    ]
                )
        , body = Http.bytesBody "application/json;charset=utf-8" config.body
        , timeout = Nothing
        , tracker = Nothing
        }


backofficeQuizIdAddTeamsTask :
    { body : Bytes.Bytes, params : { quizId : Int } }
    -> Task.Task (OpenApi.Common.Error Api.Types.BackofficeQuizIdAddTeams_Error Bytes.Bytes) Bytes.Bytes
backofficeQuizIdAddTeamsTask config =
    Http.task
        { url =
            Url.Builder.absolute
                [ "backoffice"
                , String.fromInt config.params.quizId
                , "add-teams"
                ]
                []
        , method = "POST"
        , headers = []
        , resolver =
            OpenApi.Common.bytesResolverCustom
                (Dict.fromList
                    [ ( "400"
                      , Json.Decode.map
                            Api.Types.BackofficeQuizIdAddTeams_400
                            (Json.Decode.succeed ())
                      )
                    , ( "404"
                      , Json.Decode.map
                            Api.Types.BackofficeQuizIdAddTeams_404
                            (Json.Decode.succeed ())
                      )
                    ]
                )
        , body = Http.bytesBody "application/json;charset=utf-8" config.body
        , timeout = Nothing
        }


backofficeQuizIdChangeSettings :
    { toMsg :
        Result (OpenApi.Common.Error Api.Types.BackofficeQuizIdChangeSettings_Error Bytes.Bytes) Bytes.Bytes
        -> msg
    , body : Bytes.Bytes
    , params : { quizId : Int }
    }
    -> Cmd msg
backofficeQuizIdChangeSettings config =
    Http.request
        { url =
            Url.Builder.absolute
                [ "backoffice"
                , String.fromInt config.params.quizId
                , "change-settings"
                ]
                []
        , method = "POST"
        , headers = []
        , expect =
            OpenApi.Common.expectBytesCustom
                config.toMsg
                (Dict.fromList
                    [ ( "400"
                      , Json.Decode.map
                            Api.Types.BackofficeQuizIdChangeSettings_400
                            (Json.Decode.succeed ())
                      )
                    , ( "404"
                      , Json.Decode.map
                            Api.Types.BackofficeQuizIdChangeSettings_404
                            (Json.Decode.succeed ())
                      )
                    ]
                )
        , body = Http.bytesBody "application/json;charset=utf-8" config.body
        , timeout = Nothing
        , tracker = Nothing
        }


backofficeQuizIdChangeSettingsTask :
    { body : Bytes.Bytes, params : { quizId : Int } }
    -> Task.Task (OpenApi.Common.Error Api.Types.BackofficeQuizIdChangeSettings_Error Bytes.Bytes) Bytes.Bytes
backofficeQuizIdChangeSettingsTask config =
    Http.task
        { url =
            Url.Builder.absolute
                [ "backoffice"
                , String.fromInt config.params.quizId
                , "change-settings"
                ]
                []
        , method = "POST"
        , headers = []
        , resolver =
            OpenApi.Common.bytesResolverCustom
                (Dict.fromList
                    [ ( "400"
                      , Json.Decode.map
                            Api.Types.BackofficeQuizIdChangeSettings_400
                            (Json.Decode.succeed ())
                      )
                    , ( "404"
                      , Json.Decode.map
                            Api.Types.BackofficeQuizIdChangeSettings_404
                            (Json.Decode.succeed ())
                      )
                    ]
                )
        , body = Http.bytesBody "application/json;charset=utf-8" config.body
        , timeout = Nothing
        }


backofficeQuizIdCorrectScore :
    { toMsg :
        Result (OpenApi.Common.Error Api.Types.BackofficeQuizIdCorrectScore_Error Bytes.Bytes) Bytes.Bytes
        -> msg
    , body : Bytes.Bytes
    , params : { quizId : Int }
    }
    -> Cmd msg
backofficeQuizIdCorrectScore config =
    Http.request
        { url =
            Url.Builder.absolute
                [ "backoffice"
                , String.fromInt config.params.quizId
                , "correct-score"
                ]
                []
        , method = "POST"
        , headers = []
        , expect =
            OpenApi.Common.expectBytesCustom
                config.toMsg
                (Dict.fromList
                    [ ( "400"
                      , Json.Decode.map
                            Api.Types.BackofficeQuizIdCorrectScore_400
                            (Json.Decode.succeed ())
                      )
                    , ( "404"
                      , Json.Decode.map
                            Api.Types.BackofficeQuizIdCorrectScore_404
                            (Json.Decode.succeed ())
                      )
                    ]
                )
        , body = Http.bytesBody "application/json;charset=utf-8" config.body
        , timeout = Nothing
        , tracker = Nothing
        }


backofficeQuizIdCorrectScoreTask :
    { body : Bytes.Bytes, params : { quizId : Int } }
    -> Task.Task (OpenApi.Common.Error Api.Types.BackofficeQuizIdCorrectScore_Error Bytes.Bytes) Bytes.Bytes
backofficeQuizIdCorrectScoreTask config =
    Http.task
        { url =
            Url.Builder.absolute
                [ "backoffice"
                , String.fromInt config.params.quizId
                , "correct-score"
                ]
                []
        , method = "POST"
        , headers = []
        , resolver =
            OpenApi.Common.bytesResolverCustom
                (Dict.fromList
                    [ ( "400"
                      , Json.Decode.map
                            Api.Types.BackofficeQuizIdCorrectScore_400
                            (Json.Decode.succeed ())
                      )
                    , ( "404"
                      , Json.Decode.map
                            Api.Types.BackofficeQuizIdCorrectScore_404
                            (Json.Decode.succeed ())
                      )
                    ]
                )
        , body = Http.bytesBody "application/json;charset=utf-8" config.body
        , timeout = Nothing
        }


backofficeQuizIdLock :
    { toMsg : Result (OpenApi.Common.Error () Bytes.Bytes) Bytes.Bytes -> msg
    , params : { quizId : Int }
    }
    -> Cmd msg
backofficeQuizIdLock config =
    Http.request
        { url =
            Url.Builder.absolute
                [ "backoffice", String.fromInt config.params.quizId, "lock" ]
                []
        , method = "POST"
        , headers = []
        , expect =
            OpenApi.Common.expectBytesCustom
                config.toMsg
                (Dict.fromList [ ( "404", Json.Decode.succeed () ) ])
        , body = Http.emptyBody
        , timeout = Nothing
        , tracker = Nothing
        }


backofficeQuizIdLockTask :
    { params : { quizId : Int } }
    -> Task.Task (OpenApi.Common.Error () Bytes.Bytes) Bytes.Bytes
backofficeQuizIdLockTask config =
    Http.task
        { url =
            Url.Builder.absolute
                [ "backoffice", String.fromInt config.params.quizId, "lock" ]
                []
        , method = "POST"
        , headers = []
        , resolver =
            OpenApi.Common.bytesResolverCustom
                (Dict.fromList [ ( "404", Json.Decode.succeed () ) ])
        , body = Http.emptyBody
        , timeout = Nothing
        }


backofficeQuizIdRecordRoundScores :
    { toMsg :
        Result (OpenApi.Common.Error Api.Types.BackofficeQuizIdRecordRoundScores_Error Bytes.Bytes) Bytes.Bytes
        -> msg
    , body : Bytes.Bytes
    , params : { quizId : Int }
    }
    -> Cmd msg
backofficeQuizIdRecordRoundScores config =
    Http.request
        { url =
            Url.Builder.absolute
                [ "backoffice"
                , String.fromInt config.params.quizId
                , "record-round-scores"
                ]
                []
        , method = "POST"
        , headers = []
        , expect =
            OpenApi.Common.expectBytesCustom
                config.toMsg
                (Dict.fromList
                    [ ( "400"
                      , Json.Decode.map
                            Api.Types.BackofficeQuizIdRecordRoundScores_400
                            (Json.Decode.succeed ())
                      )
                    , ( "404"
                      , Json.Decode.map
                            Api.Types.BackofficeQuizIdRecordRoundScores_404
                            (Json.Decode.succeed ())
                      )
                    ]
                )
        , body = Http.bytesBody "application/json;charset=utf-8" config.body
        , timeout = Nothing
        , tracker = Nothing
        }


backofficeQuizIdRecordRoundScoresTask :
    { body : Bytes.Bytes, params : { quizId : Int } }
    -> Task.Task (OpenApi.Common.Error Api.Types.BackofficeQuizIdRecordRoundScores_Error Bytes.Bytes) Bytes.Bytes
backofficeQuizIdRecordRoundScoresTask config =
    Http.task
        { url =
            Url.Builder.absolute
                [ "backoffice"
                , String.fromInt config.params.quizId
                , "record-round-scores"
                ]
                []
        , method = "POST"
        , headers = []
        , resolver =
            OpenApi.Common.bytesResolverCustom
                (Dict.fromList
                    [ ( "400"
                      , Json.Decode.map
                            Api.Types.BackofficeQuizIdRecordRoundScores_400
                            (Json.Decode.succeed ())
                      )
                    , ( "404"
                      , Json.Decode.map
                            Api.Types.BackofficeQuizIdRecordRoundScores_404
                            (Json.Decode.succeed ())
                      )
                    ]
                )
        , body = Http.bytesBody "application/json;charset=utf-8" config.body
        , timeout = Nothing
        }


backofficeQuizIdRenameTeam :
    { toMsg :
        Result (OpenApi.Common.Error Api.Types.BackofficeQuizIdRenameTeam_Error Bytes.Bytes) Bytes.Bytes
        -> msg
    , body : Bytes.Bytes
    , params : { quizId : Int }
    }
    -> Cmd msg
backofficeQuizIdRenameTeam config =
    Http.request
        { url =
            Url.Builder.absolute
                [ "backoffice"
                , String.fromInt config.params.quizId
                , "rename-team"
                ]
                []
        , method = "POST"
        , headers = []
        , expect =
            OpenApi.Common.expectBytesCustom
                config.toMsg
                (Dict.fromList
                    [ ( "400"
                      , Json.Decode.map
                            Api.Types.BackofficeQuizIdRenameTeam_400
                            (Json.Decode.succeed ())
                      )
                    , ( "404"
                      , Json.Decode.map
                            Api.Types.BackofficeQuizIdRenameTeam_404
                            (Json.Decode.succeed ())
                      )
                    ]
                )
        , body = Http.bytesBody "application/json;charset=utf-8" config.body
        , timeout = Nothing
        , tracker = Nothing
        }


backofficeQuizIdRenameTeamTask :
    { body : Bytes.Bytes, params : { quizId : Int } }
    -> Task.Task (OpenApi.Common.Error Api.Types.BackofficeQuizIdRenameTeam_Error Bytes.Bytes) Bytes.Bytes
backofficeQuizIdRenameTeamTask config =
    Http.task
        { url =
            Url.Builder.absolute
                [ "backoffice"
                , String.fromInt config.params.quizId
                , "rename-team"
                ]
                []
        , method = "POST"
        , headers = []
        , resolver =
            OpenApi.Common.bytesResolverCustom
                (Dict.fromList
                    [ ( "400"
                      , Json.Decode.map
                            Api.Types.BackofficeQuizIdRenameTeam_400
                            (Json.Decode.succeed ())
                      )
                    , ( "404"
                      , Json.Decode.map
                            Api.Types.BackofficeQuizIdRenameTeam_404
                            (Json.Decode.succeed ())
                      )
                    ]
                )
        , body = Http.bytesBody "application/json;charset=utf-8" config.body
        , timeout = Nothing
        }


backofficeQuizIdSetTeamActive :
    { toMsg :
        Result (OpenApi.Common.Error Api.Types.BackofficeQuizIdSetTeamActive_Error Bytes.Bytes) Bytes.Bytes
        -> msg
    , body : Bytes.Bytes
    , params : { quizId : Int }
    }
    -> Cmd msg
backofficeQuizIdSetTeamActive config =
    Http.request
        { url =
            Url.Builder.absolute
                [ "backoffice"
                , String.fromInt config.params.quizId
                , "set-team-active"
                ]
                []
        , method = "POST"
        , headers = []
        , expect =
            OpenApi.Common.expectBytesCustom
                config.toMsg
                (Dict.fromList
                    [ ( "400"
                      , Json.Decode.map
                            Api.Types.BackofficeQuizIdSetTeamActive_400
                            (Json.Decode.succeed ())
                      )
                    , ( "404"
                      , Json.Decode.map
                            Api.Types.BackofficeQuizIdSetTeamActive_404
                            (Json.Decode.succeed ())
                      )
                    ]
                )
        , body = Http.bytesBody "application/json;charset=utf-8" config.body
        , timeout = Nothing
        , tracker = Nothing
        }


backofficeQuizIdSetTeamActiveTask :
    { body : Bytes.Bytes, params : { quizId : Int } }
    -> Task.Task (OpenApi.Common.Error Api.Types.BackofficeQuizIdSetTeamActive_Error Bytes.Bytes) Bytes.Bytes
backofficeQuizIdSetTeamActiveTask config =
    Http.task
        { url =
            Url.Builder.absolute
                [ "backoffice"
                , String.fromInt config.params.quizId
                , "set-team-active"
                ]
                []
        , method = "POST"
        , headers = []
        , resolver =
            OpenApi.Common.bytesResolverCustom
                (Dict.fromList
                    [ ( "400"
                      , Json.Decode.map
                            Api.Types.BackofficeQuizIdSetTeamActive_400
                            (Json.Decode.succeed ())
                      )
                    , ( "404"
                      , Json.Decode.map
                            Api.Types.BackofficeQuizIdSetTeamActive_404
                            (Json.Decode.succeed ())
                      )
                    ]
                )
        , body = Http.bytesBody "application/json;charset=utf-8" config.body
        , timeout = Nothing
        }


backofficeQuizIdUnlock :
    { toMsg : Result (OpenApi.Common.Error () Bytes.Bytes) Bytes.Bytes -> msg
    , params : { quizId : Int }
    }
    -> Cmd msg
backofficeQuizIdUnlock config =
    Http.request
        { url =
            Url.Builder.absolute
                [ "backoffice", String.fromInt config.params.quizId, "unlock" ]
                []
        , method = "POST"
        , headers = []
        , expect =
            OpenApi.Common.expectBytesCustom
                config.toMsg
                (Dict.fromList [ ( "404", Json.Decode.succeed () ) ])
        , body = Http.emptyBody
        , timeout = Nothing
        , tracker = Nothing
        }


backofficeQuizIdUnlockTask :
    { params : { quizId : Int } }
    -> Task.Task (OpenApi.Common.Error () Bytes.Bytes) Bytes.Bytes
backofficeQuizIdUnlockTask config =
    Http.task
        { url =
            Url.Builder.absolute
                [ "backoffice", String.fromInt config.params.quizId, "unlock" ]
                []
        , method = "POST"
        , headers = []
        , resolver =
            OpenApi.Common.bytesResolverCustom
                (Dict.fromList [ ( "404", Json.Decode.succeed () ) ])
        , body = Http.emptyBody
        , timeout = Nothing
        }


public config =
    Http.request
        { url = Url.Builder.absolute [ "public" ] []
        , method = "GET"
        , headers = []
        , expect =
            OpenApi.Common.expectBytesCustom config.toMsg (Dict.fromList [])
        , body = Http.emptyBody
        , timeout = Nothing
        , tracker = Nothing
        }


publicTask : {} -> Task.Task (OpenApi.Common.Error e Bytes.Bytes) Bytes.Bytes
publicTask config =
    Http.task
        { url = Url.Builder.absolute [ "public" ] []
        , method = "GET"
        , headers = []
        , resolver = OpenApi.Common.bytesResolverCustom (Dict.fromList [])
        , body = Http.emptyBody
        , timeout = Nothing
        }


publicQuizId :
    { toMsg : Result (OpenApi.Common.Error () Bytes.Bytes) Bytes.Bytes -> msg
    , params : { quizId : Int }
    }
    -> Cmd msg
publicQuizId config =
    Http.request
        { url =
            Url.Builder.absolute
                [ "public", String.fromInt config.params.quizId ]
                []
        , method = "GET"
        , headers = []
        , expect =
            OpenApi.Common.expectBytesCustom
                config.toMsg
                (Dict.fromList [ ( "404", Json.Decode.succeed () ) ])
        , body = Http.emptyBody
        , timeout = Nothing
        , tracker = Nothing
        }


publicQuizIdTask :
    { params : { quizId : Int } }
    -> Task.Task (OpenApi.Common.Error () Bytes.Bytes) Bytes.Bytes
publicQuizIdTask config =
    Http.task
        { url =
            Url.Builder.absolute
                [ "public", String.fromInt config.params.quizId ]
                []
        , method = "GET"
        , headers = []
        , resolver =
            OpenApi.Common.bytesResolverCustom
                (Dict.fromList [ ( "404", Json.Decode.succeed () ) ])
        , body = Http.emptyBody
        , timeout = Nothing
        }
