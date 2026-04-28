module Common.HttpUtil exposing (..)

import Common.Authentication as Authentication exposing (Authentication)
import Common.Constants exposing (signatureHeader, userHeader)
import Http exposing (Error(..), Expect, expectStringResponse)
import Json.Decode as D
import Json.Encode as Encode


expectJson : (Result Http.Error a -> msg) -> D.Decoder a -> Expect msg
expectJson toMsg decoder =
    expectStringResponse toMsg <|
        \response ->
            case response of
                Http.BadUrl_ url ->
                    Err (Http.BadUrl url)

                Http.Timeout_ ->
                    Err Http.Timeout

                Http.NetworkError_ ->
                    Err Http.NetworkError

                Http.BadStatus_ _ body ->
                    Err (BadBody body)

                Http.GoodStatus_ _ body ->
                    case D.decodeString decoder body of
                        Ok value ->
                            Ok value

                        Err err ->
                            Err (BadBody (D.errorToString err))


expectWhatever : (Result Http.Error () -> msg) -> Expect msg
expectWhatever toMsg =
    expectStringResponse toMsg <|
        \response ->
            case response of
                Http.BadUrl_ url ->
                    Err (Http.BadUrl url)

                Http.Timeout_ ->
                    Err Http.Timeout

                Http.NetworkError_ ->
                    Err Http.NetworkError

                Http.BadStatus_ _ body ->
                    Err (BadBody body)

                Http.GoodStatus_ _ _ ->
                    Ok ()


postJsonWithCredentials :
    Authentication
    ->
        { url : String
        , body : Encode.Value
        , expect : Expect msg
        }
    -> Cmd msg
postJsonWithCredentials authentication request =
    let
        credentials =
            Authentication.mkCredentials authentication request.body
    in
    Http.request
        { method = "POST"
        , headers = [ Http.header userHeader credentials.user, Http.header signatureHeader credentials.signature ]
        , url = request.url
        , body = Http.jsonBody request.body
        , expect = request.expect
        , timeout = Nothing
        , tracker = Nothing
        }


get :
    { url : String
    , body : Encode.Value
    , expect : Expect msg
    }
    -> Cmd msg
get request =
    Http.request
        { method = "PUT"
        , headers = []
        , url = request.url
        , body = Http.jsonBody request.body
        , expect = request.expect
        , timeout = Nothing
        , tracker = Nothing
        }
