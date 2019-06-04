module Constants exposing ( .. )

serverLocation : String
-- serverLocation = "https://www.danilenko.io"
serverLocation = "http://localhost"

serverPort : String
-- serverPort = "8000"
serverPort = "9000"

apiLocation : String
apiLocation = mkPath [ String.concat [serverLocation, ":", serverPort], "api" ]

quizParam : String
quizParam = "quiz"

roundsParam : String
roundsParam = "rounds"

userParam : String
userParam = "user"

passwordParam : String
passwordParam = "pass"

signatureParam : String
signatureParam = "signature"

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

allApi : String
allApi = mkPath [quizApi, "all"]

loginApi : String
loginApi = mkPath [apiLocation, "secrets"]

sheetPDFPrefix : String
--sheetPDFPrefix = mkPath [ serverLocation, "quizzes" ]
sheetPDFPrefix = mkPath ["file://", "media", "nda", "DATA", "Programme", "pubquiz-server", "quizzes"]

sheetPDFFile : String
sheetPDFFile = "Sheet.pdf"