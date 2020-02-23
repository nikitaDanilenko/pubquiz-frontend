module Constants exposing ( .. )

serverLocation : String
-- serverLocation = "https://www.danilenko.io"
serverLocation = "http://localhost"

serverPort : String
-- serverPort = "8000"
serverPort = "9000"

apiLocation : String
apiLocation = mkPath [ String.concat [serverLocation, ":", serverPort], "api" ]

actionParam : String
actionParam = "action"

quizParam : String
quizParam = "quiz"

quizIdParam : String
quizIdParam = "quizId"

roundsParam : String
roundsParam = "rounds"

roundsNumberParam : String
roundsNumberParam = "roundsNumber"

newUserParam : String
newUserParam = "newUser"

userParam : String
userParam = "user"

passwordParam : String
passwordParam = "pass"

signatureParam : String
signatureParam = "signature"

quizPDNParam : String
quizPDNParam = "quizPDN"

quizSettingsParam : String
quizSettingsParam = "quizSettings"

credentialsParam : String
credentialsParam = "credentials"

numberOfTeamsParam : String
numberOfTeamsParam = "numberOfTeams"

labelsParam : String
labelsParam = "labels"

mkPath : List String -> String
mkPath = String.join "/"

quizApi : String
quizApi = mkPath [apiLocation, "quiz"]

userApi : String
userApi = mkPath [apiLocation, "users"]

newApi : String
newApi = mkPath [quizApi, "new"]

newUserApi : String
newUserApi = mkPath [userApi, "createUser"]

lockApi : String
lockApi = mkPath [quizApi, "lock"]

updateApi : String
updateApi = mkPath [quizApi, "update"]

getQuizRatingsApi : String
getQuizRatingsApi = mkPath [quizApi, "getQuizRatings"]

getLabelsApi : String
getLabelsApi = mkPath [quizApi, "getLabels"]

updateQuizSettingsApi : String
updateQuizSettingsApi = mkPath [quizApi, "updateQuizSettings"]

allApi : String
allApi = mkPath [quizApi, "all"]

loginApi : String
loginApi = mkPath [apiLocation, "secrets"]

sheetPDFPrefix : String
--sheetPDFPrefix = mkPath [ serverLocation, "quizzes" ]
sheetPDFPrefix = mkPath ["file://", "media", "nda", "DATA", "Programme", "pubquiz-server", "quizzes"]

sheetPDFFile : String
sheetPDFFile = "Sheet.pdf"

qrPDFFile : String
qrPDFFile = "QR.pdf"