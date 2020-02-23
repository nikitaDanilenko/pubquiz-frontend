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

empty : Labels
empty = {
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

default : Labels
default = mkLabels 
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