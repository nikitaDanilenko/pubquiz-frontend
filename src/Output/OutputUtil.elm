module Output.OutputUtil exposing (..)

import Common.Constants exposing (serverLocationWithFrontendPort)
import Common.Types exposing (DbQuizId, Labels, QuizIdentifier, QuizInfo)
import Url.Builder exposing (Root(..))


mkFullQuizName : QuizIdentifier -> String
mkFullQuizName idf =
    String.join " "
        [ String.concat [ idf.date, ":" ]
        , idf.name
        , String.concat [ "(", idf.place, ")" ]
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
