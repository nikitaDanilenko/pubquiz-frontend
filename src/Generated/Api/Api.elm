module Api.Api exposing
    ( backoffice, backofficeLogin, backofficeLoginTask, backofficeTask, public, publicQuizId, publicQuizIdTask
    , publicTask
    )

{-|


## Operations

@docs backoffice, backofficeLogin, backofficeLoginTask, backofficeTask, public, publicQuizId, publicQuizIdTask
@docs publicTask

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


backoffice config =
    Http.request
        { url = Url.Builder.absolute [ "backoffice" ] []
        , method = "POST"
        , headers = []
        , expect =
            OpenApi.Common.expectJsonCustom
                config.toMsg
                (Dict.fromList [ ( "400", Json.Decode.succeed () ) ])
                Api.Json.decodeQuizActive
        , body = Http.jsonBody (Api.Json.encodeQuizMetaData config.body)
        , timeout = Nothing
        , tracker = Nothing
        }


backofficeTask :
    { body : Api.Types.QuizMetaData }
    -> Task.Task (OpenApi.Common.Error () String) Api.Types.QuizActive
backofficeTask config =
    Http.task
        { url = Url.Builder.absolute [ "backoffice" ] []
        , method = "POST"
        , headers = []
        , resolver =
            OpenApi.Common.jsonResolverCustom
                (Dict.fromList [ ( "400", Json.Decode.succeed () ) ])
                Api.Json.decodeQuizActive
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
                Api.Json.decodeLoginResponse
        , body = Http.jsonBody (Api.Json.encodeLoginRequest config.body)
        , timeout = Nothing
        , tracker = Nothing
        }


backofficeLoginTask :
    { body : Api.Types.LoginRequest }
    -> Task.Task (OpenApi.Common.Error () String) Api.Types.LoginResponse
backofficeLoginTask config =
    Http.task
        { url = Url.Builder.absolute [ "backoffice", "login" ] []
        , method = "POST"
        , headers = []
        , resolver =
            OpenApi.Common.jsonResolverCustom
                (Dict.fromList [ ( "400", Json.Decode.succeed () ) ])
                Api.Json.decodeLoginResponse
        , body = Http.jsonBody (Api.Json.encodeLoginRequest config.body)
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
    { toMsg :
        Result (OpenApi.Common.Error () String) Api.Types.QuizActive -> msg
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
                Api.Json.decodeQuizActive
        , body = Http.emptyBody
        , timeout = Nothing
        , tracker = Nothing
        }


publicQuizIdTask :
    { params : { quizId : Int } }
    -> Task.Task (OpenApi.Common.Error () String) Api.Types.QuizActive
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
                Api.Json.decodeQuizActive
        , body = Http.emptyBody
        , timeout = Nothing
        }
