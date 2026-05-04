#!/bin/bash
# Generate API from OpenAPI spec and apply post-processing fixes

set -e

# Load environment variables
source .env

# Generate API
elm-open-api "$OPENAPI_URL" --output-dir src/Generated --module-name Api

# Post-processing fix for elm-open-api empty body bug
# See: https://github.com/wolfadex/elm-open-api/issues/14
node scripts/fix-openapi-empty-body.js

echo "API generation complete"
