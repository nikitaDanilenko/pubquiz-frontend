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

defaultLabels : Labels
defaultLabels = {
      roundLabel = "Runde", 
      groupLabel = "Gruppe", 
      ownPointsLabel = "Erreichte Punkte", 
      maxReachedLabel = "Erreichte Höchstpunktzahl", 
      maxReachableLabel = "Erreichbare Punkte", 
      backToChartView = "Gesamtwertung", 
      mainLabel = "Quiz", 
      ownPageLabel = "Eigene Punkte"
    }

toParams : Labels -> List (String, String)
toParams labels = [
  ("roundLabel", labels.roundLabel),
  ("groupLabel", labels.groupLabel),
  ("ownPointsLabel", labels.ownPointsLabel),
  ("maxReachedLabel", labels.maxReachedLabel),
  ("maxReachableLabel", labels.maxReachableLabel),
  ("backToChartView", labels.backToChartView),
  ("mainLabel", labels.mainLabel),
  ("ownPageLabel", labels.ownPageLabel)
  ]