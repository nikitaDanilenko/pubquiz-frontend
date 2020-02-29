module Input.Validity exposing ( .. )
-- todo: check necessity
type alias Validity = 
    {
        serverTextOK : Bool,
        pointsValid : Bool,
        teamNamesValid : Bool
    }

default : Validity
default = {
        serverTextOK = False,
        pointsValid = True,
        teamNamesValid = True
    }

isValid : Validity -> Bool
isValid v = List.all (\x -> x) [v.serverTextOK, v.pointsValid, v.teamNamesValid]

updatePoints : Bool -> Validity -> Validity
updatePoints b v = { v | pointsValid = b }

updateServerText : Bool -> Validity -> Validity
updateServerText b v = { v | serverTextOK = b }

updateTeamNames : Bool -> Validity -> Validity
updateTeamNames b v = { v | teamNamesValid = b }