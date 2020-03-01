module Common.Constants exposing ( .. )

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


quizIdParam : String
quizIdParam = "quizId"


newUserParam : String
newUserParam = "newUser"

userParam : String
userParam = "user"

passwordParam : String
passwordParam = "pass"

signatureParam : String
signatureParam = "signature"

quizIdentifierParam : String
quizIdentifierParam = "quizIdentifier"

quizSettingsParam : String
quizSettingsParam = "quizSettings"

quizRatingsParam : String
quizRatingsParam = "quizRatings"

credentialsParam : String
credentialsParam = "credentials"

teamQueryParam : String
teamQueryParam = "teamQuery"

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

teamTableApi : String
teamTableApi = mkPath [quizApi, "teamTable"]

allApi : String
allApi = mkPath [quizApi, "all"]

loginApi : String
loginApi = mkPath [apiLocation, "secrets"]

sheetPDFPrefix : String
--sheetPDFPrefix = mkPath [ serverLocation ]
sheetPDFPrefix = mkPath ["file://", "media", "nda", "DATA", "Programme", "pubquiz-server"]