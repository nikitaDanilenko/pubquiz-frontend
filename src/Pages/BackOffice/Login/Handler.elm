module Pages.BackOffice.Login.Handler exposing (init, update)

{-| Back office login page logic.
-}

import Api.Api
import Api.Types exposing (LoginRequest)
import OpenApi.Common
import Pages.BackOffice.Login.Page as Page


init : ( Page.Model, Cmd Page.Msg )
init =
    ( { username = ""
      , password = ""
      , error = Nothing
      , isSubmitting = False
      }
    , Cmd.none
    )


update : Page.Msg -> Page.Model -> ( Page.Model, Cmd Page.Msg, Bool )
update msg model =
    case msg of
        Page.SetUsername username ->
            ( Page.lenses.username.set username model
            , Cmd.none
            , False
            )

        Page.SetPassword password ->
            ( Page.lenses.password.set password model
            , Cmd.none
            , False
            )

        Page.Submit ->
            ( model
                |> Page.lenses.isSubmitting.set True
                |> Page.lenses.error.set Nothing
            , login model.username model.password
            , False
            )

        Page.GotLoginResponse result ->
            case result of
                Ok _ ->
                    ( model |> Page.lenses.isSubmitting.set False
                    , Cmd.none
                    , True
                    )

                Err error ->
                    ( model
                        |> Page.lenses.isSubmitting.set False
                        |> Page.lenses.error.set (Just (errorToString error))
                    , Cmd.none
                    , False
                    )


login : String -> String -> Cmd Page.Msg
login username password =
    Api.Api.backofficeLogin
        { toMsg = Page.GotLoginResponse
        , body = LoginRequest password username
        }


errorToString : OpenApi.Common.Error () String -> String
errorToString error =
    case error of
        OpenApi.Common.BadUrl _ ->
            "Invalid URL"

        OpenApi.Common.Timeout ->
            "Request timed out"

        OpenApi.Common.NetworkError ->
            "Network error"

        OpenApi.Common.KnownBadStatus status _ ->
            if status == 401 then
                "Invalid username or password"

            else
                String.concat [ "Error: ", String.fromInt status ]

        OpenApi.Common.UnknownBadStatus _ body ->
            String.concat [ "Error: ", body ]

        OpenApi.Common.BadErrorBody _ body ->
            String.concat [ "Error: ", body ]

        OpenApi.Common.BadBody _ body ->
            String.concat [ "Error: ", body ]
