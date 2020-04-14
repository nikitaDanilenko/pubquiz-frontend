module Output.OutputUtil exposing (..)

import Common.Constants exposing (serverLocationWithFrontendPort)
import Common.Types exposing (DbQuizId, Labels, QuizIdentifier, QuizInfo)
import Url.Builder exposing (Root(..))


mkFullQuizName : QuizIdentifier -> String
mkFullQuizName idf =
    String.join " "
        [ if String.isEmpty idf.date then "" else String.concat [ idf.date, ":" ]
        , idf.name
        , if String.isEmpty idf.place then "" else String.concat [ "(", idf.place, ")" ]
        ]


fragmentUrl : List String -> String
fragmentUrl parts =
    Url.Builder.custom Relative [] [] (Just (Url.Builder.absolute parts []))


fromServerUrl : List String -> List String -> String
fromServerUrl pathSegments parts =
    Url.Builder.custom
        (CrossOrigin serverLocationWithFrontendPort)
        (pathSegments ++ [ "" ])
        []
        (Just (Url.Builder.absolute parts []))
