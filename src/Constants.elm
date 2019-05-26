module Constants exposing ( .. )

apiLocation : String
--apiLocation = "https://www.danilenko.io:8000/api"
apiLocation = "http://localhost:9000/api"

quizParam : String
quizParam = "quiz"

roundsParam : String
roundsParam = "rounds"

userParam : String
userParam = "user"

passwordParam : String
passwordParam = "pass"

mkPath : List String -> String
mkPath = String.join "/"

quizApi : String
quizApi = mkPath [apiLocation, "quiz"]

newApi : String
newApi = mkPath [quizApi, "new"]

lockApi : String
lockApi = mkPath [quizApi, "lock"]

updateApi : String
updateApi = mkPath [quizApi, "update"]

allApi : String
allApi = mkPath [quizApi, "all"]

loginApi : String
loginApi = mkPath [apiLocation, "secrets"]