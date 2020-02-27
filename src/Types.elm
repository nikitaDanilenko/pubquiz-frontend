module Types exposing(..)

import Json.Decode
import Json.Encode exposing (Value)
-- The following module comes from bartavelle/json-helpers
import Json.Helpers exposing (..)
import Dict exposing (Dict)
import Set exposing (Set)


type alias TeamNumber  = Int

jsonDecTeamNumber : Json.Decode.Decoder ( TeamNumber )
jsonDecTeamNumber =
    Json.Decode.int

jsonEncTeamNumber : TeamNumber -> Value
jsonEncTeamNumber  val = Json.Encode.int val



type alias RoundNumber  = Int

jsonDecRoundNumber : Json.Decode.Decoder ( RoundNumber )
jsonDecRoundNumber =
    Json.Decode.int

jsonEncRoundNumber : RoundNumber -> Value
jsonEncRoundNumber  val = Json.Encode.int val



type alias Code  = String

jsonDecCode : Json.Decode.Decoder ( Code )
jsonDecCode =
    Json.Decode.string

jsonEncCode : Code -> Value
jsonEncCode  val = Json.Encode.string val



type alias TeamName  = String

jsonDecTeamName : Json.Decode.Decoder ( TeamName )
jsonDecTeamName =
    Json.Decode.string

jsonEncTeamName : TeamName -> Value
jsonEncTeamName  val = Json.Encode.string val



type alias QuizName  = String

jsonDecQuizName : Json.Decode.Decoder ( QuizName )
jsonDecQuizName =
    Json.Decode.string

jsonEncQuizName : QuizName -> Value
jsonEncQuizName  val = Json.Encode.string val



type alias Place  = String

jsonDecPlace : Json.Decode.Decoder ( Place )
jsonDecPlace =
    Json.Decode.string

jsonEncPlace : Place -> Value
jsonEncPlace  val = Json.Encode.string val



type alias QuizDate  = Day

jsonDecQuizDate : Json.Decode.Decoder ( QuizDate )
jsonDecQuizDate =
    jsonDecDay

jsonEncQuizDate : QuizDate -> Value
jsonEncQuizDate  val = jsonEncDay val



type alias RoundLabel  = String

jsonDecRoundLabel : Json.Decode.Decoder ( RoundLabel )
jsonDecRoundLabel =
    Json.Decode.string

jsonEncRoundLabel : RoundLabel -> Value
jsonEncRoundLabel  val = Json.Encode.string val



type alias TeamLabel  = String

jsonDecTeamLabel : Json.Decode.Decoder ( TeamLabel )
jsonDecTeamLabel =
    Json.Decode.string

jsonEncTeamLabel : TeamLabel -> Value
jsonEncTeamLabel  val = Json.Encode.string val



type alias OwnPointsLabel  = String

jsonDecOwnPointsLabel : Json.Decode.Decoder ( OwnPointsLabel )
jsonDecOwnPointsLabel =
    Json.Decode.string

jsonEncOwnPointsLabel : OwnPointsLabel -> Value
jsonEncOwnPointsLabel  val = Json.Encode.string val



type alias MaxReachedLabel  = String

jsonDecMaxReachedLabel : Json.Decode.Decoder ( MaxReachedLabel )
jsonDecMaxReachedLabel =
    Json.Decode.string

jsonEncMaxReachedLabel : MaxReachedLabel -> Value
jsonEncMaxReachedLabel  val = Json.Encode.string val



type alias MaxReachableLabel  = String

jsonDecMaxReachableLabel : Json.Decode.Decoder ( MaxReachableLabel )
jsonDecMaxReachableLabel =
    Json.Decode.string

jsonEncMaxReachableLabel : MaxReachableLabel -> Value
jsonEncMaxReachableLabel  val = Json.Encode.string val



type alias BackToChartViewLabel  = String

jsonDecBackToChartViewLabel : Json.Decode.Decoder ( BackToChartViewLabel )
jsonDecBackToChartViewLabel =
    Json.Decode.string

jsonEncBackToChartViewLabel : BackToChartViewLabel -> Value
jsonEncBackToChartViewLabel  val = Json.Encode.string val



type alias OwnPageLabel  = String

jsonDecOwnPageLabel : Json.Decode.Decoder ( OwnPageLabel )
jsonDecOwnPageLabel =
    Json.Decode.string

jsonEncOwnPageLabel : OwnPageLabel -> Value
jsonEncOwnPageLabel  val = Json.Encode.string val



type alias ViewPreviousLabel  = String

jsonDecViewPreviousLabel : Json.Decode.Decoder ( ViewPreviousLabel )
jsonDecViewPreviousLabel =
    Json.Decode.string

jsonEncViewPreviousLabel : ViewPreviousLabel -> Value
jsonEncViewPreviousLabel  val = Json.Encode.string val



type alias CumulativeLabel  = String

jsonDecCumulativeLabel : Json.Decode.Decoder ( CumulativeLabel )
jsonDecCumulativeLabel =
    Json.Decode.string

jsonEncCumulativeLabel : CumulativeLabel -> Value
jsonEncCumulativeLabel  val = Json.Encode.string val



type alias IndividualRoundsLabel  = String

jsonDecIndividualRoundsLabel : Json.Decode.Decoder ( IndividualRoundsLabel )
jsonDecIndividualRoundsLabel =
    Json.Decode.string

jsonEncIndividualRoundsLabel : IndividualRoundsLabel -> Value
jsonEncIndividualRoundsLabel  val = Json.Encode.string val



type alias ProgressionLabel  = String

jsonDecProgressionLabel : Json.Decode.Decoder ( ProgressionLabel )
jsonDecProgressionLabel =
    Json.Decode.string

jsonEncProgressionLabel : ProgressionLabel -> Value
jsonEncProgressionLabel  val = Json.Encode.string val



type alias PlacementLabel  = String

jsonDecPlacementLabel : Json.Decode.Decoder ( PlacementLabel )
jsonDecPlacementLabel =
    Json.Decode.string

jsonEncPlacementLabel : PlacementLabel -> Value
jsonEncPlacementLabel  val = Json.Encode.string val



type alias PlaceLabel  = String

jsonDecPlaceLabel : Json.Decode.Decoder ( PlaceLabel )
jsonDecPlaceLabel =
    Json.Decode.string

jsonEncPlaceLabel : PlaceLabel -> Value
jsonEncPlaceLabel  val = Json.Encode.string val



type alias PointsLabel  = String

jsonDecPointsLabel : Json.Decode.Decoder ( PointsLabel )
jsonDecPointsLabel =
    Json.Decode.string

jsonEncPointsLabel : PointsLabel -> Value
jsonEncPointsLabel  val = Json.Encode.string val



type alias RoundWinnerLabel  = String

jsonDecRoundWinnerLabel : Json.Decode.Decoder ( RoundWinnerLabel )
jsonDecRoundWinnerLabel =
    Json.Decode.string

jsonEncRoundWinnerLabel : RoundWinnerLabel -> Value
jsonEncRoundWinnerLabel  val = Json.Encode.string val



type alias UserName  = String

jsonDecUserName : Json.Decode.Decoder ( UserName )
jsonDecUserName =
    Json.Decode.string

jsonEncUserName : UserName -> Value
jsonEncUserName  val = Json.Encode.string val



type alias UserSalt  = String

jsonDecUserSalt : Json.Decode.Decoder ( UserSalt )
jsonDecUserSalt =
    Json.Decode.string

jsonEncUserSalt : UserSalt -> Value
jsonEncUserSalt  val = Json.Encode.string val



type alias UserHash  = String

jsonDecUserHash : Json.Decode.Decoder ( UserHash )
jsonDecUserHash =
    Json.Decode.string

jsonEncUserHash : UserHash -> Value
jsonEncUserHash  val = Json.Encode.string val



type alias Day  = String

jsonDecDay : Json.Decode.Decoder ( Day )
jsonDecDay =
    Json.Decode.string

jsonEncDay : Day -> Value
jsonEncDay  val = Json.Encode.string val



type alias TeamRating  =
   { teamNumber: TeamNumber
   , rating: Float
   }

jsonDecTeamRating : Json.Decode.Decoder ( TeamRating )
jsonDecTeamRating =
   Json.Decode.succeed (\pteamNumber prating -> {teamNumber = pteamNumber, rating = prating})
   |> required "teamNumber" (jsonDecTeamNumber)
   |> required "rating" (Json.Decode.float)

jsonEncTeamRating : TeamRating -> Value
jsonEncTeamRating  val =
   Json.Encode.object
   [ ("teamNumber", jsonEncTeamNumber val.teamNumber)
   , ("rating", Json.Encode.float val.rating)
   ]



type alias RoundRating  =
   { reachableInRound: Float
   , points: (List TeamRating)
   }

jsonDecRoundRating : Json.Decode.Decoder ( RoundRating )
jsonDecRoundRating =
   Json.Decode.succeed (\preachableInRound ppoints -> {reachableInRound = preachableInRound, points = ppoints})
   |> required "reachableInRound" (Json.Decode.float)
   |> required "points" (Json.Decode.list (jsonDecTeamRating))

jsonEncRoundRating : RoundRating -> Value
jsonEncRoundRating  val =
   Json.Encode.object
   [ ("reachableInRound", Json.Encode.float val.reachableInRound)
   , ("points", (Json.Encode.list jsonEncTeamRating) val.points)
   ]



type alias Ratings  = (List (RoundNumber, RoundRating))

jsonDecRatings : Json.Decode.Decoder ( Ratings )
jsonDecRatings =
    Json.Decode.list (Json.Decode.map2 tuple2 (Json.Decode.index 0 (jsonDecRoundNumber)) (Json.Decode.index 1 (jsonDecRoundRating)))

jsonEncRatings : Ratings -> Value
jsonEncRatings  val = (Json.Encode.list (\(t1,t2) -> Json.Encode.list identity [(jsonEncRoundNumber) t1,(jsonEncRoundRating) t2])) val



type alias Credentials  =
   { user: UserName
   , signature: UserHash
   }

jsonDecCredentials : Json.Decode.Decoder ( Credentials )
jsonDecCredentials =
   Json.Decode.succeed (\puser psignature -> {user = puser, signature = psignature})
   |> required "user" (jsonDecUserName)
   |> required "signature" (jsonDecUserHash)

jsonEncCredentials : Credentials -> Value
jsonEncCredentials  val =
   Json.Encode.object
   [ ("user", jsonEncUserName val.user)
   , ("signature", jsonEncUserHash val.signature)
   ]



type alias QuizSettings  =
   { rounds: (List Int)
   , numberOfTeams: Int
   , labels: Labels
   }

jsonDecQuizSettings : Json.Decode.Decoder ( QuizSettings )
jsonDecQuizSettings =
   Json.Decode.succeed (\prounds pnumberOfTeams plabels -> {rounds = prounds, numberOfTeams = pnumberOfTeams, labels = plabels})
   |> required "rounds" (Json.Decode.list (Json.Decode.int))
   |> required "numberOfTeams" (Json.Decode.int)
   |> required "labels" (jsonDecLabels)

jsonEncQuizSettings : QuizSettings -> Value
jsonEncQuizSettings  val =
   Json.Encode.object
   [ ("rounds", (Json.Encode.list Json.Encode.int) val.rounds)
   , ("numberOfTeams", Json.Encode.int val.numberOfTeams)
   , ("labels", jsonEncLabels val.labels)
   ]



type alias QuizIdentifier  =
   { place: Place
   , date: QuizDate
   , name: QuizName
   }

jsonDecQuizIdentifier : Json.Decode.Decoder ( QuizIdentifier )
jsonDecQuizIdentifier =
   Json.Decode.succeed (\pplace pdate pname -> {place = pplace, date = pdate, name = pname})
   |> required "place" (jsonDecPlace)
   |> required "date" (jsonDecQuizDate)
   |> required "name" (jsonDecQuizName)

jsonEncQuizIdentifier : QuizIdentifier -> Value
jsonEncQuizIdentifier  val =
   Json.Encode.object
   [ ("place", jsonEncPlace val.place)
   , ("date", jsonEncQuizDate val.date)
   , ("name", jsonEncQuizName val.name)
   ]



type alias QuizInfo  =
   { quizId: DbQuizId
   , quizIdentifier: QuizIdentifier
   , active: Activity
   , fullSheetPath: String
   , qrOnlyPath: String
   }

jsonDecQuizInfo : Json.Decode.Decoder ( QuizInfo )
jsonDecQuizInfo =
   Json.Decode.succeed (\pquizId pquizIdentifier pactive pfullSheetPath pqrOnlyPath -> {quizId = pquizId, quizIdentifier = pquizIdentifier, active = pactive, fullSheetPath = pfullSheetPath, qrOnlyPath = pqrOnlyPath})
   |> required "quizId" (jsonDecDbQuizId)
   |> required "quizIdentifier" (jsonDecQuizIdentifier)
   |> required "active" (jsonDecActivity)
   |> required "fullSheetPath" (Json.Decode.string)
   |> required "qrOnlyPath" (Json.Decode.string)

jsonEncQuizInfo : QuizInfo -> Value
jsonEncQuizInfo  val =
   Json.Encode.object
   [ ("quizId", jsonEncDbQuizId val.quizId)
   , ("quizIdentifier", jsonEncQuizIdentifier val.quizIdentifier)
   , ("active", jsonEncActivity val.active)
   , ("fullSheetPath", Json.Encode.string val.fullSheetPath)
   , ("qrOnlyPath", Json.Encode.string val.qrOnlyPath)
   ]



type alias Labels  =
   { roundLabel: RoundLabel
   , teamLabel: TeamLabel
   , ownPointsLabel: OwnPointsLabel
   , maxReachedLabel: MaxReachedLabel
   , maxReachableLabel: MaxReachableLabel
   , backToChartView: BackToChartViewLabel
   , ownPageLabel: OwnPageLabel
   , viewPrevious: ViewPreviousLabel
   , cumulativeLabel: CumulativeLabel
   , individualRoundsLabel: IndividualRoundsLabel
   , progressionLabel: ProgressionLabel
   , placementLabel: PlacementLabel
   , placeLabel: PlaceLabel
   , pointsLabel: PointsLabel
   , roundWinnerLabel: RoundWinnerLabel
   }

jsonDecLabels : Json.Decode.Decoder ( Labels )
jsonDecLabels =
   Json.Decode.succeed (\proundLabel pteamLabel pownPointsLabel pmaxReachedLabel pmaxReachableLabel pbackToChartView pownPageLabel pviewPrevious pcumulativeLabel pindividualRoundsLabel pprogressionLabel pplacementLabel pplaceLabel ppointsLabel proundWinnerLabel -> {roundLabel = proundLabel, teamLabel = pteamLabel, ownPointsLabel = pownPointsLabel, maxReachedLabel = pmaxReachedLabel, maxReachableLabel = pmaxReachableLabel, backToChartView = pbackToChartView, ownPageLabel = pownPageLabel, viewPrevious = pviewPrevious, cumulativeLabel = pcumulativeLabel, individualRoundsLabel = pindividualRoundsLabel, progressionLabel = pprogressionLabel, placementLabel = pplacementLabel, placeLabel = pplaceLabel, pointsLabel = ppointsLabel, roundWinnerLabel = proundWinnerLabel})
   |> required "roundLabel" (jsonDecRoundLabel)
   |> required "teamLabel" (jsonDecTeamLabel)
   |> required "ownPointsLabel" (jsonDecOwnPointsLabel)
   |> required "maxReachedLabel" (jsonDecMaxReachedLabel)
   |> required "maxReachableLabel" (jsonDecMaxReachableLabel)
   |> required "backToChartView" (jsonDecBackToChartViewLabel)
   |> required "ownPageLabel" (jsonDecOwnPageLabel)
   |> required "viewPrevious" (jsonDecViewPreviousLabel)
   |> required "cumulativeLabel" (jsonDecCumulativeLabel)
   |> required "individualRoundsLabel" (jsonDecIndividualRoundsLabel)
   |> required "progressionLabel" (jsonDecProgressionLabel)
   |> required "placementLabel" (jsonDecPlacementLabel)
   |> required "placeLabel" (jsonDecPlaceLabel)
   |> required "pointsLabel" (jsonDecPointsLabel)
   |> required "roundWinnerLabel" (jsonDecRoundWinnerLabel)

jsonEncLabels : Labels -> Value
jsonEncLabels  val =
   Json.Encode.object
   [ ("roundLabel", jsonEncRoundLabel val.roundLabel)
   , ("teamLabel", jsonEncTeamLabel val.teamLabel)
   , ("ownPointsLabel", jsonEncOwnPointsLabel val.ownPointsLabel)
   , ("maxReachedLabel", jsonEncMaxReachedLabel val.maxReachedLabel)
   , ("maxReachableLabel", jsonEncMaxReachableLabel val.maxReachableLabel)
   , ("backToChartView", jsonEncBackToChartViewLabel val.backToChartView)
   , ("ownPageLabel", jsonEncOwnPageLabel val.ownPageLabel)
   , ("viewPrevious", jsonEncViewPreviousLabel val.viewPrevious)
   , ("cumulativeLabel", jsonEncCumulativeLabel val.cumulativeLabel)
   , ("individualRoundsLabel", jsonEncIndividualRoundsLabel val.individualRoundsLabel)
   , ("progressionLabel", jsonEncProgressionLabel val.progressionLabel)
   , ("placementLabel", jsonEncPlacementLabel val.placementLabel)
   , ("placeLabel", jsonEncPlaceLabel val.placeLabel)
   , ("pointsLabel", jsonEncPointsLabel val.pointsLabel)
   , ("roundWinnerLabel", jsonEncRoundWinnerLabel val.roundWinnerLabel)
   ]



type alias DbQuizId  = Int

jsonDecDbQuizId : Json.Decode.Decoder ( DbQuizId )
jsonDecDbQuizId =
    Json.Decode.int

jsonEncDbQuizId : DbQuizId -> Value
jsonEncDbQuizId  val = Json.Encode.int val



type alias Password  = String

jsonDecPassword : Json.Decode.Decoder ( Password )
jsonDecPassword =
    Json.Decode.string

jsonEncPassword : Password -> Value
jsonEncPassword  val = Json.Encode.string val



type alias TeamInfo  =
   { teamInfoCode: Code
   , teamInfoName: TeamName
   , teamInfoNumber: TeamNumber
   , teamInfoActivity: Activity
   }

jsonDecTeamInfo : Json.Decode.Decoder ( TeamInfo )
jsonDecTeamInfo =
   Json.Decode.succeed (\pteamInfoCode pteamInfoName pteamInfoNumber pteamInfoActivity -> {teamInfoCode = pteamInfoCode, teamInfoName = pteamInfoName, teamInfoNumber = pteamInfoNumber, teamInfoActivity = pteamInfoActivity})
   |> required "teamInfoCode" (jsonDecCode)
   |> required "teamInfoName" (jsonDecTeamName)
   |> required "teamInfoNumber" (jsonDecTeamNumber)
   |> required "teamInfoActivity" (jsonDecActivity)

jsonEncTeamInfo : TeamInfo -> Value
jsonEncTeamInfo  val =
   Json.Encode.object
   [ ("teamInfoCode", jsonEncCode val.teamInfoCode)
   , ("teamInfoName", jsonEncTeamName val.teamInfoName)
   , ("teamInfoNumber", jsonEncTeamNumber val.teamInfoNumber)
   , ("teamInfoActivity", jsonEncActivity val.teamInfoActivity)
   ]



type alias Header  = (List TeamInfo)

jsonDecHeader : Json.Decode.Decoder ( Header )
jsonDecHeader =
    Json.Decode.list (jsonDecTeamInfo)

jsonEncHeader : Header -> Value
jsonEncHeader  val = (Json.Encode.list jsonEncTeamInfo) val



type Activity  =
    Active 
    | Inactive 

jsonDecActivity : Json.Decode.Decoder ( Activity )
jsonDecActivity = 
    let jsonDecDictActivity = Dict.fromList [("Active", Active), ("Inactive", Inactive)]
    in  decodeSumUnaries "Activity" jsonDecDictActivity

jsonEncActivity : Activity -> Value
jsonEncActivity  val =
    case val of
        Active -> Json.Encode.string "Active"
        Inactive -> Json.Encode.string "Inactive"



type Action  =
    CreateQuizA 
    | LockA 
    | UpdateSettingsA 

jsonDecAction : Json.Decode.Decoder ( Action )
jsonDecAction = 
    let jsonDecDictAction = Dict.fromList [("CreateQuizA", CreateQuizA), ("LockA", LockA), ("UpdateSettingsA", UpdateSettingsA)]
    in  decodeSumUnaries "Action" jsonDecDictAction

jsonEncAction : Action -> Value
jsonEncAction  val =
    case val of
        CreateQuizA -> Json.Encode.string "CreateQuizA"
        LockA -> Json.Encode.string "LockA"
        UpdateSettingsA -> Json.Encode.string "UpdateSettingsA"



type alias QuizRatings  =
   { header: Header
   , ratings: Ratings
   }

jsonDecQuizRatings : Json.Decode.Decoder ( QuizRatings )
jsonDecQuizRatings =
   Json.Decode.succeed (\pheader pratings -> {header = pheader, ratings = pratings})
   |> required "header" (jsonDecHeader)
   |> required "ratings" (jsonDecRatings)

jsonEncQuizRatings : QuizRatings -> Value
jsonEncQuizRatings  val =
   Json.Encode.object
   [ ("header", jsonEncHeader val.header)
   , ("ratings", jsonEncRatings val.ratings)
   ]

