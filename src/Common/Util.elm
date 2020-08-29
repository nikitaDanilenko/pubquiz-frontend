module Common.Util exposing (..)

import Common.Constants exposing (quizIdParam)
import Common.HttpUtil as HttpUtil
import Common.Types exposing (DbQuizId, Labels, QuizInfo, QuizRatings, TeamRating, jsonDecQuizInfo, jsonEncDbQuizId)
import Http exposing (Error)
import Json.Decode as Decode
import Json.Encode as Encode exposing (Value)
import List.Extra
import Url.Builder exposing (string)


escapeHTML : String -> String
escapeHTML str =
    case String.uncons str of
        Nothing ->
            ""

        Just ( c, cs ) ->
            let
                escapedC =
                    case c of
                        'ä' ->
                            "&auml;"

                        'Ä' ->
                            "&Auml;"

                        'ö' ->
                            "&ouml;"

                        'Ö' ->
                            "&Ouml;"

                        'ü' ->
                            "&uuml;"

                        'Ü' ->
                            "&Uuml;"

                        'ß' ->
                            "&szlig;"

                        _ ->
                            String.fromChar c
            in
            String.concat [ escapedC, escapeHTML cs ]


foldMaybe : b -> (a -> b) -> Maybe a -> b
foldMaybe empty f m =
    Maybe.withDefault empty (Maybe.map f m)


foldResult : b -> (a -> b) -> Result e a -> b
foldResult empty f r =
    Result.withDefault empty (Result.map f r)


foldResultWith : (e -> b) -> (a -> b) -> Result e a -> b
foldResultWith errorFunction okFunction result =
    case result of
        Ok value ->
            okFunction value

        Err error ->
            errorFunction error


getMsg : String -> (Result Error a -> msg) -> Decode.Decoder a -> DbQuizId -> Cmd msg
getMsg =
    getMsgWith jsonEncDbQuizId quizIdParam


getMsgWith : (v -> Value) -> String -> String -> (Result Error a -> msg) -> Decode.Decoder a -> v -> Cmd msg
getMsgWith encoder param path action decoder v =
    Http.get
        { url = Url.Builder.relative [ path ] [ string param (Encode.encode 0 (encoder v)) ]
        , expect = HttpUtil.expectJson action decoder
        }


getAllWith : String -> (Result Error (List QuizInfo) -> msg) -> Cmd msg
getAllWith pathToAllApi mkMsg =
    Http.get
        { url = pathToAllApi
        , expect = HttpUtil.expectJson mkMsg (Decode.list jsonDecQuizInfo)
        }


intersectWith : (a -> b -> c) -> (x -> Int) -> (x -> a) -> (y -> Int) -> (y -> b) -> List x -> List y -> List c
intersectWith combine xKey xValue yKey yValue =
    let
        merge : List x -> List y -> List c
        merge xs ys =
            case ( xs, ys ) of
                ( [], _ ) ->
                    []

                ( _, [] ) ->
                    []

                ( hx :: tx, hy :: ty ) ->
                    let
                        xK =
                            xKey hx

                        yK =
                            yKey hy
                    in
                    if xK == yK then
                        combine (xValue hx) (yValue hy) :: merge tx ty

                    else if xK < yK then
                        merge tx ys

                    else
                        merge xs ty
    in
    merge


groupBy : (a -> a -> Bool) -> List a -> List (List a)
groupBy equal l =
    case l of
        [] ->
            []

        x :: xs ->
            List.Extra.takeWhile (equal x) l :: groupBy equal (List.Extra.dropWhile (equal x) xs)


isDefined : Maybe a -> Bool
isDefined m =
    case m of
        Just _ ->
            True

        Nothing ->
            False


type alias ErrorOr a =
    Result Error a

special : Int -> String
special =
    Char.fromCode >> String.fromChar