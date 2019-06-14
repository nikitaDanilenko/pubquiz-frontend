module Labels exposing ( .. )

type alias Labels = {
      roundLabel : String,
      groupLabel : String,
      ownPointsLabel : String, 
      maxReachedLabel : String,
      maxReachableLabel : String,
      backToChartView : String,
      mainLabel : String,
      ownPageLabel : String
    }

emptyLabels : Labels
emptyLabels = {
      roundLabel = "",
      groupLabel = "",
      ownPointsLabel = "", 
      maxReachedLabel = "",
      maxReachableLabel = "",
      backToChartView = "",
      mainLabel = "",
      ownPageLabel = ""   
    }

mkLabels : String -> String -> String -> String -> String -> String -> String -> String -> Labels
mkLabels roundLbl groupLbl ownPointsLbl maxReachedLbl maxReachableLbl backToChartLbl mainLbl ownPageLbl =
  {
    roundLabel = roundLbl,
    groupLabel = groupLbl,
    ownPointsLabel = ownPointsLbl,
    maxReachedLabel = maxReachedLbl,
    maxReachableLabel = maxReachableLbl,
    backToChartView = backToChartLbl,
    mainLabel = mainLbl,
    ownPageLabel = ownPageLbl
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

toParams : Labels -> List (String, String)
toParams labels = [
  ("roundLabel", labels.roundLabel),
  ("groupLabel", labels.groupLabel),
  ("ownPointsLabel", labels.ownPointsLabel),
  ("maxReachedLabel", labels.maxReachedLabel),
  ("maxReachableLabel", labels.maxReachableLabel),
  ("backToChartViewLabel", labels.backToChartView),
  ("mainLabel", labels.mainLabel),
  ("ownPageLabel", labels.ownPageLabel)
  ]