module Common.Constants exposing ( .. )

serverLocation : String
-- serverLocation = "https://www.danilenko.io"
serverLocation = "http://localhost"

serverBackendPort : String
-- serverBackendPort = "8000"
serverBackendPort = "9000"

serverLocationWithBackendPort : String
serverLocationWithBackendPort = String.join ":" [serverLocation, serverBackendPort]

localBuild : Bool
localBuild = True

serverLocationWithFrontendPort : String
serverLocationWithFrontendPort =
  let serverFrontendPort = "9876"
  in String.join ":" (serverLocation :: if localBuild then [serverFrontendPort] else [])

apiLocation : String
apiLocation = mkPath [ serverLocationWithBackendPort, "api" ]


quizIdParam : String
quizIdParam = "quizId"

teamNumberParam : String
teamNumberParam = "teamNumber"

teamCodeParam : String
teamCodeParam = "teamCode"

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

getQuizSettingsApi : String
getQuizSettingsApi = mkPath [quizApi, "getQuizSettings"]

allApi : String
allApi = mkPath [quizApi, "all"]

allActiveApi : String
allActiveApi = mkPath [quizApi, "allActive"]

loginApi : String
loginApi = mkPath [apiLocation, "secrets"]

serverQuizzesFolder : String
serverQuizzesFolder = "quizzes"

userHeader : String
userHeader = "User"

signatureHeader : String
signatureHeader = "Signature"