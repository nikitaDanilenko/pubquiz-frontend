module Base exposing ( .. )

import Date exposing (Date, day, fromCalendarDate, monthNumber, numberToMonth, year)
import Types exposing (Day)

type alias SessionKey = String

dayToDate : Day -> Date
dayToDate d = fromCalendarDate d.year (numberToMonth d.month) d.day

dateToDay : Date -> Day
dateToDay date = {
    year = year date,
    month = monthNumber date,
    day = day date
    }