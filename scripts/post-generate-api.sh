#!/bin/sh
# Post-generation cleanup for elm-open-api generated Elm code

set -e

DIR="$(cd "$(dirname "$0")/.." && pwd)"
cd "$DIR"

# Fix module names to include Generated prefix (module declarations)
find src/Generated -name "*.elm" -exec sed -i '' 's/^module Api\./module Generated.Api./' {} \;
find src/Generated -name "*.elm" -exec sed -i '' 's/^module OpenApi\./module Generated.OpenApi./' {} \;

# Fix import statements
find src/Generated -name "*.elm" -exec sed -i '' 's/^import Api\./import Generated.Api./' {} \;
find src/Generated -name "*.elm" -exec sed -i '' 's/^import OpenApi\./import Generated.OpenApi./' {} \;

# Fix inline references to Api.Types and OpenApi.Common
find src/Generated -name "*.elm" -exec sed -i '' 's/Api\.Types\./Generated.Api.Types./g' {} \;
find src/Generated -name "*.elm" -exec sed -i '' 's/OpenApi\.Common\./Generated.OpenApi.Common./g' {} \;

# Fix ScoreBoard.scores type - Map (Int, Int) Double -> List ScoreEntry
# Add ScoreEntry type and update ScoreBoard
sed -i '' 's/import Json.Encode/import Json.Encode\n\n\ntype alias ScoreEntry =\n    { teamNumber : Int, roundNumber : Int, points : Float }/' src/Generated/Api/Types.elm
sed -i '' 's/scores : List Json.Encode.Value/scores : List ScoreEntry/g' src/Generated/Api/Types.elm

# Export ScoreEntry in the module
sed -i '' 's/, Round, ScoreBoard, SetTeamActiveCommand/, Round, ScoreBoard, ScoreEntry, SetTeamActiveCommand/' src/Generated/Api/Types.elm
sed -i '' 's/@docs Round, ScoreBoard, SetTeamActiveCommand/@docs Round, ScoreBoard, ScoreEntry, SetTeamActiveCommand/' src/Generated/Api/Types.elm
