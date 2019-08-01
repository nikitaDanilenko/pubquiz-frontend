module Validity exposing ( .. )

type alias Validity = 
    {
        serverTextOK : Bool,
        pointsValid : Bool
    }

default : Validity
default = {
        serverTextOK = False,
        pointsValid = True
    }

isValid : Validity -> Bool
isValid v = List.all (\x -> x) [v.serverTextOK, v.pointsValid]

updatePoints : Bool -> Validity -> Validity
updatePoints b v = { v | pointsValid = b }

updateServerText : Bool -> Validity -> Validity
updateServerText b v = { v | serverTextOK = b }