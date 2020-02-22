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

createQuiz : String
createQuiz = "createQuiz"

labelUpdate : String
labelUpdate = "labelUpdate"

lockQuiz : String
lockQuiz = "lock"

quizParam : String
quizParam = "quiz"

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

dataParam : String
dataParam = "data"

quizPDNParam : String
quizPDNParam = "quizPDN"

quizSettingsParam : String
quizSettingsParam = "quizSettings"

credentialsParam : String
credentialsParam = "credentials"

numberOfTeamsParam : String
numberOfTeamsParam = "numberOfTeams"

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