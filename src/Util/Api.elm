module Util.Api exposing (errorToString)

import OpenApi.Common


errorToString : (Int -> String) -> OpenApi.Common.Error e String -> String
errorToString onKnownBadStatus error =
    case error of
        OpenApi.Common.BadUrl _ ->
            "Invalid URL"

        OpenApi.Common.Timeout ->
            "Request timed out"

        OpenApi.Common.NetworkError ->
            "Network error"

        OpenApi.Common.KnownBadStatus status _ ->
            onKnownBadStatus status

        OpenApi.Common.UnknownBadStatus _ body ->
            String.concat [ "Error: ", body ]

        OpenApi.Common.BadErrorBody _ body ->
            String.concat [ "Error: ", body ]

        OpenApi.Common.BadBody _ body ->
            String.concat [ "Error: ", body ]
