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

teamNumberParam : String
teamNumberParam = "teamNumber"

teamCodeParam : String
teamCodeParam = "teamCode"

userParam : String
userParam = "user"

passwordParam : String
passwordParam = "pass"

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

userCreationParam : String
userCreationParam = "userCreation"

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

updateQuizRatingsApi : String
updateQuizRatingsApi = mkPath [quizApi, "updateQuizRatings"]

getQuizRatingsApi : String
getQuizRatingsApi = mkPath [quizApi, "getQuizRatings"]

getLabelsApi : String
getLabelsApi = mkPath [quizApi, "getLabels"]

getQuizInfoApi : String
getQuizInfoApi = mkPath [quizApi, "getQuizInfo"]

updateQuizApi : String
updateQuizApi = mkPath [quizApi, "updateQuiz"]

teamTableApi : String
teamTableApi = mkPath [quizApi, "teamTable"]

allApi : String
allApi = mkPath [quizApi, "all"]

loginApi : String
loginApi = mkPath [apiLocation, "secrets"]

-- todo: remove the port.
sheetPDFPrefix : String
sheetPDFPrefix = String.join ":" [ serverLocation, "9876" ]