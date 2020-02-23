module Base exposing ( .. )

import Date exposing (Date, fromIsoString, toIsoString)
import Types exposing (Day)

type alias SessionKey = String

dayToDate : Day -> Result String Date
dayToDate = fromIsoString

dateToDay : Date -> Day
dateToDay = toIsoString