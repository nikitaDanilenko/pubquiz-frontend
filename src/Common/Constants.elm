module Common.Constants exposing ( .. )

serverLocation : String
-- serverLocation = "https://www.danilenko.io"
serverLocation = "http://localhost"

serverBackendPort : String
-- serverBackendPort = "8000"
serverBackendPort = "9000"

-- todo: Adjust this port.
serverFrontendPort : String
serverFrontendPort = "9876"

serverLocationWithBackendPort : String
serverLocationWithBackendPort = String.join ":" [serverLocation, serverBackendPort]

serverLocationWithFrontendPort : String
serverLocationWithFrontendPort = String.join ":" [serverLocation, serverFrontendPort]

apiLocation : String
apiLocation = mkPath [ serverLocationWithBackendPort, "api" ]

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

getQuizSettingsApi : String
getQuizSettingsApi = mkPath [quizApi, "getQuizSettings"]

allApi : String
allApi = mkPath [quizApi, "all"]

loginApi : String
loginApi = mkPath [apiLocation, "secrets"]

serverQuizzesFolder : String
serverQuizzesFolder = "quizzes"