module Common.Util exposing (..)

import Common.Constants exposing (allApi, quizIdParam)
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



{- Fills a list with zeroes in the back if the list is not long enough,
   otherwise return the prefix of the list with the given length.
-}


adjustToSize : Int -> List TeamRating -> List TeamRating
adjustToSize n =
    adjustToSizeWith (List.indexedMap (\i r -> { teamNumber = i, rating = r }) (List.repeat n 0))


adjustToSizeWith : List a -> List a -> List a
adjustToSizeWith dft lst =
    let
        combine : List a -> List a -> List a
        combine l r =
            case ( l, r ) of
                ( [], rest ) ->
                    rest

                ( _, [] ) ->
                    []

                ( x :: xs, _ :: ys ) ->
                    x :: combine xs ys
    in
    combine lst dft


updateIndex : Int -> a -> List a -> List a
updateIndex i y =
    List.indexedMap
        (\j x ->
            if i == j then
                y

            else
                x
        )


isValidQuizName : String -> Bool
isValidQuizName n =
    not (String.isEmpty n)


find : (a -> Bool) -> List a -> Maybe a
find p l =
    case l of
        [] ->
            Nothing

        x :: xs ->
            if p x then
                Just x

            else
                find p xs


getMsg : String -> (Result Error a -> msg) -> Decode.Decoder a -> DbQuizId -> Cmd msg
getMsg =
    getMsgWith jsonEncDbQuizId quizIdParam


getMsgWith : (v -> Value) -> String -> String -> (Result Error a -> msg) -> Decode.Decoder a -> v -> Cmd msg
getMsgWith encoder param path action decoder v =
    Http.get
        { url = Url.Builder.relative [ path ] [ string param (Encode.encode 0 (encoder v)) ]
        , expect = Http.expectJson action decoder
        }


getAllWith : (Result Error (List QuizInfo) -> msg) -> Cmd msg
getAllWith mkMsg =
    Http.get
        { url = allApi
        , expect = Http.expectJson mkMsg (Decode.list jsonDecQuizInfo)
        }


uncurry3 : (a -> b -> c -> d) -> ( a, b, c ) -> d
uncurry3 f ( a, b, c ) =
    f a b c


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
