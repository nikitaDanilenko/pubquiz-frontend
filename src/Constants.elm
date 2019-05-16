module Constants exposing ( .. )

apiLocation : String
--apiLocation = "https://www.danilenko.io:8000/api/"
apiLocation = "http://localhost:9000/api/"

quizParam : String
quizParam = "quiz"

roundsParam : String
roundsParam = "rounds"

userParam : String
userParam = "user"

passwordParam : String
passwordParam = "pass"

quizApi : String
quizApi = String.concat [ apiLocation, "quiz"]

newApi : String
newApi = "create"

lockApi : String
lockApi = "lock"

updateApi : String
updateApi = "update"

allApi : String
allApi = "all"

loginApi : String
loginApi = 