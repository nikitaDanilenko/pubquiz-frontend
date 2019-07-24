module Labels exposing ( .. )

type alias Labels = {
      roundLabel : String,
      teamLabel : String,
      ownPointsLabel : String, 
      maxReachedLabel : String,
      maxReachableLabel : String,
      backToChartView : String,
      mainLabel : String,
      ownPageLabel : String,
      viewQuizzesLabel : String,
      cumulativeLabel : String,
      individualLabel : String,
      progressionLabel : String,
      placementLabel : String,
      placeLabel : String,
      pointsLabel : String,
      roundWinnerLabel : String
    }

emptyLabels : Labels
emptyLabels = {
      roundLabel = "",
      teamLabel = "",
      ownPointsLabel = "", 
      maxReachedLabel = "",
      maxReachableLabel = "",
      backToChartView = "",
      mainLabel = "",
      ownPageLabel = "",
      viewQuizzesLabel = "",
      cumulativeLabel = "",
      individualLabel = "",
      progressionLabel = "",
      placementLabel = "",
      placeLabel = "",
      pointsLabel = "",
      roundWinnerLabel = ""
    }

parseLabels : String -> Maybe Labels
parseLabels text = 
  let lines = String.lines text
  in case lines of
       r :: t :: op :: mred :: mr :: btc :: m :: o :: vq :: c :: i :: pr :: plcmt :: plc :: pts :: rw :: [] ->
        Just (mkLabels r t op mred mr btc m o vq c i pr plcmt plc pts rw)
       _ -> Nothing

mkLabels : String 
        -> String 
        -> String
        -> String
        -> String
        -> String
        -> String
        -> String
        -> String
        -> String
        -> String
        -> String
        -> String
        -> String
        -> String
        -> String
        -> Labels
mkLabels roundLbl teamLbl ownPointsLbl maxReachedLbl maxReachableLbl backToChartLbl mainLbl
         ownPageLbl viewQuizzesLbl cumulativeLbl individualLbl progressionLbl
         placementLbl placeLbl pointsLbl roundWinnerLbl =
  {
    roundLabel = roundLbl,
    teamLabel = teamLbl,
    ownPointsLabel = ownPointsLbl,
    maxReachedLabel = maxReachedLbl,
    maxReachableLabel = maxReachableLbl,
    backToChartView = backToChartLbl,
    mainLabel = mainLbl,
    ownPageLabel = ownPageLbl,
    viewQuizzesLabel = viewQuizzesLbl,
    cumulativeLabel = cumulativeLbl,
    individualLabel = individualLbl,
    progressionLabel = progressionLbl,
    placementLabel = placementLbl,
    placeLabel = placeLbl,
    pointsLabel = pointsLbl,
    roundWinnerLabel = roundWinnerLbl
  }

defaultLabels : Labels
defaultLabels = mkLabels 
    "Runde"
    "Gruppe"
    "Erreichte Punkte"
    (String.concat ["Erreichte H", String.fromChar (Char.fromCode 246), "chstpunktzahl"])
    "Erreichbare Punkte"
    "Gesamtwertung"
    "Quiz"
    "Eigene Punkte"
    "Alle Quizzes"
    "Gesamtpunktzahl"
    "Punkte pro Runde"
    "Verlauf"
    "Platzierung"
    "Platz"
    "Punkte"
    "Rundensieger"

toParams : Labels -> List (String, String)
toParams labels = [
  ("roundLabel", labels.roundLabel),
  ("teamLabel", labels.teamLabel),
  ("ownPointsLabel", labels.ownPointsLabel),
  ("maxReachedLabel", labels.maxReachedLabel),
  ("maxReachableLabel", labels.maxReachableLabel),
  ("backToChartViewLabel", labels.backToChartView),
  ("mainLabel", labels.mainLabel),
  ("ownPageLabel", labels.ownPageLabel),
  ("viewQuizzesLabel", labels.viewQuizzesLabel),
  ("cumulativeLabel", labels.cumulativeLabel),
  ("individualLabel", labels.individualLabel),
  ("progressionLabel", labels.progressionLabel),
  ("placementLabel", labels.placementLabel),
  ("placeLabel", labels.placeLabel),
  ("pointsLabel", labels.pointsLabel),
  ("roundWinnerLabel", labels.roundWinnerLabel)
  ]