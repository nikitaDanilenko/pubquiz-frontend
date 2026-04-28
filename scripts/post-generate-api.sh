#!/bin/sh
# Post-generation cleanup for OpenAPI generated Elm code
# Removes scaffolding files and flattens the src/ directory

set -e

DIR="$(cd "$(dirname "$0")/.." && pwd)"
cd "$DIR"

# Remove project scaffolding files
rm -rf src/Generated/elm.json \
       src/Generated/.gitignore \
       src/Generated/README.md \
       src/Generated/.openapi-generator-ignore \
       src/Generated/.openapi-generator

# Flatten the nested src/ directory
mv src/Generated/src/* src/Generated/
rmdir src/Generated/src

