module Api.Api exposing
    ( backoffice, backofficeLogin, backofficeLoginTask, backofficeQuizIdAddTeams, backofficeQuizIdAddTeamsTask
    , backofficeQuizIdChangeSettings, backofficeQuizIdChangeSettingsTask, backofficeQuizIdCorrectScore
    , backofficeQuizIdCorrectScoreTask, backofficeQuizIdLock, backofficeQuizIdLockTask
    , backofficeQuizIdRecordRoundScores, backofficeQuizIdRecordRoundScoresTask, backofficeQuizIdRenameTeam
    , backofficeQuizIdRenameTeamTask, backofficeQuizIdSetTeamActive, backofficeQuizIdSetTeamActiveTask
    , backofficeQuizIdUnlock, backofficeQuizIdUnlockTask, backofficeTask, backofficeWhoami, backofficeWhoamiTask
    , public, publicQuizId, publicQuizIdTask, publicTask
    )

{-|


## Operations

@docs backoffice, backofficeLogin, backofficeLoginTask, backofficeQuizIdAddTeams, backofficeQuizIdAddTeamsTask
@docs backofficeQuizIdChangeSettings, backofficeQuizIdChangeSettingsTask, backofficeQuizIdCorrectScore
@docs backofficeQuizIdCorrectScoreTask, backofficeQuizIdLock, backofficeQuizIdLockTask
@docs backofficeQuizIdRecordRoundScores, backofficeQuizIdRecordRoundScoresTask, backofficeQuizIdRenameTeam
@docs backofficeQuizIdRenameTeamTask, backofficeQuizIdSetTeamActive, backofficeQuizIdSetTeamActiveTask
@docs backofficeQuizIdUnlock, backofficeQuizIdUnlockTask, backofficeTask, backofficeWhoami, backofficeWhoamiTask
@docs public, publicQuizId, publicQuizIdTask, publicTask

-}

import Api.Json
import Api.Types
import Dict
import Http
import Json.Decode
import Json.Encode
import OpenApi.Common
import Task
import Url.Builder


{-| **Requires authentication.** Call `/backoffice/login` first; the returned cookie is sent automatically.
-}
backoffice config =
    Http.request
        { url = Url.Builder.absolute [ "backoffice" ] []
        , method = "POST"
        , headers = []
        , expect =
            OpenApi.Common.expectJsonCustom
                config.toMsg
                (Dict.fromList [ ( "400", Json.Decode.succeed () ) ])
                Api.Json.decodeQuiz
        , body = Http.jsonBody (Api.Json.encodeQuizMetaData config.body)
        , timeout = Nothing
        , tracker = Nothing
        }


{-| **Requires authentication.** Call `/backoffice/login` first; the returned cookie is sent automatically.
-}
backofficeTask :
    { body : Api.Types.QuizMetaData }
    -> Task.Task (OpenApi.Common.Error () String) Api.Types.Quiz
backofficeTask config =
    Http.task
        { url = Url.Builder.absolute [ "backoffice" ] []
        , method = "POST"
        , headers = []
        , resolver =
            OpenApi.Common.jsonResolverCustom
                (Dict.fromList [ ( "400", Json.Decode.succeed () ) ])
                Api.Json.decodeQuiz
        , body = Http.jsonBody (Api.Json.encodeQuizMetaData config.body)
        , timeout = Nothing
        }


backofficeLogin config =
    Http.request
        { url = Url.Builder.absolute [ "backoffice", "login" ] []
        , method = "POST"
        , headers = []
        , expect =
            OpenApi.Common.expectJsonCustom
                config.toMsg
                (Dict.fromList [ ( "400", Json.Decode.succeed () ) ])
                (Json.Decode.succeed ())
        , body = Http.jsonBody (Api.Json.encodeLoginRequest config.body)
        , timeout = Nothing
        , tracker = Nothing
        }


backofficeLoginTask :
    { body : Api.Types.LoginRequest }
    -> Task.Task (OpenApi.Common.Error () String) ()
backofficeLoginTask config =
    Http.task
        { url = Url.Builder.absolute [ "backoffice", "login" ] []
        , method = "POST"
        , headers = []
        , resolver =
            OpenApi.Common.jsonResolverCustom
                (Dict.fromList [ ( "400", Json.Decode.succeed () ) ])
                (Json.Decode.succeed ())
        , body = Http.jsonBody (Api.Json.encodeLoginRequest config.body)
        , timeout = Nothing
        }


{-| **Requires authentication.** Call `/backoffice/login` first; the returned cookie is sent automatically.
-}
backofficeWhoami config =
    Http.request
        { url = Url.Builder.absolute [ "backoffice", "whoami" ] []
        , method = "GET"
        , headers = []
        , expect =
            OpenApi.Common.expectJsonCustom
                config.toMsg
                (Dict.fromList [])
                Api.Json.decodeAuthenticatedUser
        , body = Http.emptyBody
        , timeout = Nothing
        , tracker = Nothing
        }


{-| **Requires authentication.** Call `/backoffice/login` first; the returned cookie is sent automatically.
-}
backofficeWhoamiTask : {} -> Task.Task (OpenApi.Common.Error e String) Api.Types.AuthenticatedUser
backofficeWhoamiTask config =
    Http.task
        { url = Url.Builder.absolute [ "backoffice", "whoami" ] []
        , method = "GET"
        , headers = []
        , resolver =
            OpenApi.Common.jsonResolverCustom
                (Dict.fromList [])
                Api.Json.decodeAuthenticatedUser
        , body = Http.emptyBody
        , timeout = Nothing
        }


{-| **Requires authentication.** Call `/backoffice/login` first; the returned cookie is sent automatically.
-}
backofficeQuizIdAddTeams :
    { toMsg :
        Result (OpenApi.Common.Error Api.Types.BackofficeQuizIdAddTeams_Error String) ()
        -> msg
    , body : Api.Types.AddTeamsCommand
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
            OpenApi.Common.expectJsonCustom
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
                (Json.Decode.succeed ())
        , body = Http.jsonBody (Api.Json.encodeAddTeamsCommand config.body)
        , timeout = Nothing
        , tracker = Nothing
        }


{-| **Requires authentication.** Call `/backoffice/login` first; the returned cookie is sent automatically.
-}
backofficeQuizIdAddTeamsTask :
    { body : Api.Types.AddTeamsCommand, params : { quizId : Int } }
    -> Task.Task (OpenApi.Common.Error Api.Types.BackofficeQuizIdAddTeams_Error String) ()
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
            OpenApi.Common.jsonResolverCustom
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
                (Json.Decode.succeed ())
        , body = Http.jsonBody (Api.Json.encodeAddTeamsCommand config.body)
        , timeout = Nothing
        }


{-| **Requires authentication.** Call `/backoffice/login` first; the returned cookie is sent automatically.
-}
backofficeQuizIdChangeSettings :
    { toMsg :
        Result (OpenApi.Common.Error Api.Types.BackofficeQuizIdChangeSettings_Error String) ()
        -> msg
    , body : Api.Types.ChangeSettingsCommand
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
            OpenApi.Common.expectJsonCustom
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
                (Json.Decode.succeed ())
        , body =
            Http.jsonBody (Api.Json.encodeChangeSettingsCommand config.body)
        , timeout = Nothing
        , tracker = Nothing
        }


{-| **Requires authentication.** Call `/backoffice/login` first; the returned cookie is sent automatically.
-}
backofficeQuizIdChangeSettingsTask :
    { body : Api.Types.ChangeSettingsCommand, params : { quizId : Int } }
    -> Task.Task (OpenApi.Common.Error Api.Types.BackofficeQuizIdChangeSettings_Error String) ()
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
            OpenApi.Common.jsonResolverCustom
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
                (Json.Decode.succeed ())
        , body =
            Http.jsonBody (Api.Json.encodeChangeSettingsCommand config.body)
        , timeout = Nothing
        }


{-| **Requires authentication.** Call `/backoffice/login` first; the returned cookie is sent automatically.
-}
backofficeQuizIdCorrectScore :
    { toMsg :
        Result (OpenApi.Common.Error Api.Types.BackofficeQuizIdCorrectScore_Error String) ()
        -> msg
    , body : Api.Types.CorrectScoreCommand
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
            OpenApi.Common.expectJsonCustom
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
                (Json.Decode.succeed ())
        , body = Http.jsonBody (Api.Json.encodeCorrectScoreCommand config.body)
        , timeout = Nothing
        , tracker = Nothing
        }


{-| **Requires authentication.** Call `/backoffice/login` first; the returned cookie is sent automatically.
-}
backofficeQuizIdCorrectScoreTask :
    { body : Api.Types.CorrectScoreCommand, params : { quizId : Int } }
    -> Task.Task (OpenApi.Common.Error Api.Types.BackofficeQuizIdCorrectScore_Error String) ()
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
            OpenApi.Common.jsonResolverCustom
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
                (Json.Decode.succeed ())
        , body = Http.jsonBody (Api.Json.encodeCorrectScoreCommand config.body)
        , timeout = Nothing
        }


{-| **Requires authentication.** Call `/backoffice/login` first; the returned cookie is sent automatically.
-}
backofficeQuizIdLock :
    { toMsg : Result (OpenApi.Common.Error () String) () -> msg
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
            OpenApi.Common.expectJsonCustom
                config.toMsg
                (Dict.fromList [ ( "404", Json.Decode.succeed () ) ])
                (Json.Decode.succeed ())
        , body = Http.emptyBody
        , timeout = Nothing
        , tracker = Nothing
        }


{-| **Requires authentication.** Call `/backoffice/login` first; the returned cookie is sent automatically.
-}
backofficeQuizIdLockTask :
    { params : { quizId : Int } }
    -> Task.Task (OpenApi.Common.Error () String) ()
backofficeQuizIdLockTask config =
    Http.task
        { url =
            Url.Builder.absolute
                [ "backoffice", String.fromInt config.params.quizId, "lock" ]
                []
        , method = "POST"
        , headers = []
        , resolver =
            OpenApi.Common.jsonResolverCustom
                (Dict.fromList [ ( "404", Json.Decode.succeed () ) ])
                (Json.Decode.succeed ())
        , body = Http.emptyBody
        , timeout = Nothing
        }


{-| **Requires authentication.** Call `/backoffice/login` first; the returned cookie is sent automatically.
-}
backofficeQuizIdRecordRoundScores :
    { toMsg :
        Result (OpenApi.Common.Error Api.Types.BackofficeQuizIdRecordRoundScores_Error String) ()
        -> msg
    , body : Api.Types.RecordRoundScoresCommand
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
            OpenApi.Common.expectJsonCustom
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
                (Json.Decode.succeed ())
        , body =
            Http.jsonBody (Api.Json.encodeRecordRoundScoresCommand config.body)
        , timeout = Nothing
        , tracker = Nothing
        }


{-| **Requires authentication.** Call `/backoffice/login` first; the returned cookie is sent automatically.
-}
backofficeQuizIdRecordRoundScoresTask :
    { body : Api.Types.RecordRoundScoresCommand, params : { quizId : Int } }
    -> Task.Task (OpenApi.Common.Error Api.Types.BackofficeQuizIdRecordRoundScores_Error String) ()
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
            OpenApi.Common.jsonResolverCustom
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
                (Json.Decode.succeed ())
        , body =
            Http.jsonBody (Api.Json.encodeRecordRoundScoresCommand config.body)
        , timeout = Nothing
        }


{-| **Requires authentication.** Call `/backoffice/login` first; the returned cookie is sent automatically.
-}
backofficeQuizIdRenameTeam :
    { toMsg :
        Result (OpenApi.Common.Error Api.Types.BackofficeQuizIdRenameTeam_Error String) ()
        -> msg
    , body : Api.Types.RenameTeamCommand
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
            OpenApi.Common.expectJsonCustom
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
                (Json.Decode.succeed ())
        , body = Http.jsonBody (Api.Json.encodeRenameTeamCommand config.body)
        , timeout = Nothing
        , tracker = Nothing
        }


{-| **Requires authentication.** Call `/backoffice/login` first; the returned cookie is sent automatically.
-}
backofficeQuizIdRenameTeamTask :
    { body : Api.Types.RenameTeamCommand, params : { quizId : Int } }
    -> Task.Task (OpenApi.Common.Error Api.Types.BackofficeQuizIdRenameTeam_Error String) ()
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
            OpenApi.Common.jsonResolverCustom
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
                (Json.Decode.succeed ())
        , body = Http.jsonBody (Api.Json.encodeRenameTeamCommand config.body)
        , timeout = Nothing
        }


{-| **Requires authentication.** Call `/backoffice/login` first; the returned cookie is sent automatically.
-}
backofficeQuizIdSetTeamActive :
    { toMsg :
        Result (OpenApi.Common.Error Api.Types.BackofficeQuizIdSetTeamActive_Error String) ()
        -> msg
    , body : Api.Types.SetTeamActiveCommand
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
            OpenApi.Common.expectJsonCustom
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
                (Json.Decode.succeed ())
        , body = Http.jsonBody (Api.Json.encodeSetTeamActiveCommand config.body)
        , timeout = Nothing
        , tracker = Nothing
        }


{-| **Requires authentication.** Call `/backoffice/login` first; the returned cookie is sent automatically.
-}
backofficeQuizIdSetTeamActiveTask :
    { body : Api.Types.SetTeamActiveCommand, params : { quizId : Int } }
    -> Task.Task (OpenApi.Common.Error Api.Types.BackofficeQuizIdSetTeamActive_Error String) ()
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
            OpenApi.Common.jsonResolverCustom
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
                (Json.Decode.succeed ())
        , body = Http.jsonBody (Api.Json.encodeSetTeamActiveCommand config.body)
        , timeout = Nothing
        }


{-| **Requires authentication.** Call `/backoffice/login` first; the returned cookie is sent automatically.
-}
backofficeQuizIdUnlock :
    { toMsg : Result (OpenApi.Common.Error () String) () -> msg
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
            OpenApi.Common.expectJsonCustom
                config.toMsg
                (Dict.fromList [ ( "404", Json.Decode.succeed () ) ])
                (Json.Decode.succeed ())
        , body = Http.emptyBody
        , timeout = Nothing
        , tracker = Nothing
        }


{-| **Requires authentication.** Call `/backoffice/login` first; the returned cookie is sent automatically.
-}
backofficeQuizIdUnlockTask :
    { params : { quizId : Int } }
    -> Task.Task (OpenApi.Common.Error () String) ()
backofficeQuizIdUnlockTask config =
    Http.task
        { url =
            Url.Builder.absolute
                [ "backoffice", String.fromInt config.params.quizId, "unlock" ]
                []
        , method = "POST"
        , headers = []
        , resolver =
            OpenApi.Common.jsonResolverCustom
                (Dict.fromList [ ( "404", Json.Decode.succeed () ) ])
                (Json.Decode.succeed ())
        , body = Http.emptyBody
        , timeout = Nothing
        }


public config =
    Http.request
        { url = Url.Builder.absolute [ "public" ] []
        , method = "GET"
        , headers = []
        , expect =
            OpenApi.Common.expectJsonCustom
                config.toMsg
                (Dict.fromList [])
                (Json.Decode.list Api.Json.decodeQuizSummary)
        , body = Http.emptyBody
        , timeout = Nothing
        , tracker = Nothing
        }


publicTask : {} -> Task.Task (OpenApi.Common.Error e String) (List Api.Types.QuizSummary)
publicTask config =
    Http.task
        { url = Url.Builder.absolute [ "public" ] []
        , method = "GET"
        , headers = []
        , resolver =
            OpenApi.Common.jsonResolverCustom
                (Dict.fromList [])
                (Json.Decode.list Api.Json.decodeQuizSummary)
        , body = Http.emptyBody
        , timeout = Nothing
        }


publicQuizId :
    { toMsg : Result (OpenApi.Common.Error () String) Api.Types.Quiz -> msg
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
            OpenApi.Common.expectJsonCustom
                config.toMsg
                (Dict.fromList [ ( "404", Json.Decode.succeed () ) ])
                Api.Json.decodeQuiz
        , body = Http.emptyBody
        , timeout = Nothing
        , tracker = Nothing
        }


publicQuizIdTask :
    { params : { quizId : Int } }
    -> Task.Task (OpenApi.Common.Error () String) Api.Types.Quiz
publicQuizIdTask config =
    Http.task
        { url =
            Url.Builder.absolute
                [ "public", String.fromInt config.params.quizId ]
                []
        , method = "GET"
        , headers = []
        , resolver =
            OpenApi.Common.jsonResolverCustom
                (Dict.fromList [ ( "404", Json.Decode.succeed () ) ])
                Api.Json.decodeQuiz
        , body = Http.emptyBody
        , timeout = Nothing
        }
