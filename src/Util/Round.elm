module Util.Round exposing (publishedRounds)

import Api.Types exposing (Round)


publishedRounds : List Round -> List Round
publishedRounds =
    List.filter .published >> List.sortBy .number
