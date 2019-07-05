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
      progressionLabel : String
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
      progressionLabel = ""
    }

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
        -> Labels
mkLabels roundLbl teamLbl ownPointsLbl maxReachedLbl maxReachableLbl backToChartLbl mainLbl
         ownPageLbl viewQuizzesLbl cumulativeLbl individualLbl progressionLbl =
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
    progressionLabel = progressionLbl
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
  ("progressionLabel", labels.progressionLabel)
  ]