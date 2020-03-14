module Input.CreateUser exposing (..)

import Basics.Extra exposing (flip)
import Common.Authentication exposing (Authentication)
import Common.ConnectionUtil exposing (addFeedbackLabel, encodeBody, errorToString)
import Common.Constants exposing (newUserApi, userCreationParam)
import Common.Types exposing (jsonEncUserCreation)
import Html exposing (Html, button, div, input, label, text)
import Html.Attributes exposing (class, disabled, for, id, type_)
import Html.Events exposing (onClick, onInput)
import Html.Events.Extra exposing (onEnter)
import Http
import Input.Model exposing (ErrorOr)
import Input.NewUser as NewUser exposing (NewUser, NewUserField(..))
import Input.RequestUtils exposing (encodeWithSignature)


type alias Model =
    { newUser : NewUser
    , authentication : Authentication
    , feedback : String
    }


updateNewUser : Model -> NewUser -> Model
updateNewUser model newUser =
    { model | newUser = newUser }


updateFeedback : Model -> String -> Model
updateFeedback model feedback =
    { model | feedback = feedback }


type Msg
    = SetNewUserField NewUserField String
    | CreateUser
    | CreatedUser (ErrorOr ())
    | Back


init : Authentication -> Model
init authentication =
    { newUser = NewUser.empty
    , authentication = authentication
    , feedback = ""
    }


view : Model -> Html Msg
view md =
    let
        createOnEnter =
            onEnter CreateUser
    in
    div [ id "creatingUserView" ]
        [ div [ id "creatingUser" ]
            [ label [ for "username" ] [ text "User name" ]
            , input [ onInput (SetNewUserField UserField), createOnEnter ] []
            ]
        , div [ id "creatingPassword1" ]
            [ label [ for "password1" ] [ text "Password" ]
            , input [ onInput (SetNewUserField PasswordField1), type_ "password", createOnEnter ] []
            ]
        , div [ id "creatingPassword2" ]
            [ label [ for "password2" ] [ text "Repeat password" ]
            , input [ onInput (SetNewUserField PasswordField2), type_ "password", createOnEnter ] []
            ]
        , button
            [ class "button"
            , onClick CreateUser
            , disabled (not (NewUser.isValid md.newUser))
            ]
            [ text "Create" ]
        , button [ class "backButton", onClick Back ] [ text "Back" ]
        , addFeedbackLabel md.feedback
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetNewUserField newUserField string ->
            let
                newModel =
                    NewUser.update model.newUser newUserField string
                        |> updateNewUser model
            in
            ( newModel, Cmd.none )

        CreateUser ->
            ( model, createNewUser model.authentication model.newUser )

        CreatedUser errorOr ->
            case errorOr of
                Ok _ ->
                    let
                        newModel =
                            model
                                |> flip updateFeedback (String.join " " [ "Created user", model.newUser.user ])
                                |> flip updateNewUser NewUser.empty
                    in
                    ( newModel, Cmd.none )

                Err error ->
                    let
                        feedback =
                            String.join " "
                                [ "Failed to create user"
                                , model.newUser.user
                                , "."
                                , "Reason:"
                                , errorToString error
                                ]
                    in
                    ( updateFeedback model feedback, Cmd.none )

        Back ->
            ( model, Cmd.none )


createNewUser : Authentication -> NewUser -> Cmd Msg
createNewUser authentication newUser =
    let
        params =
            encodeWithSignature authentication
                [ ( userCreationParam, jsonEncUserCreation { userCreationUser = newUser.user, userCreationPassword = newUser.password1 } ) ]
    in
    Http.post
        { url = newUserApi
        , body = encodeBody params
        , expect = Http.expectWhatever CreatedUser
        }
