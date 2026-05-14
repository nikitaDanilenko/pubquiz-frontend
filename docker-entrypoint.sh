#!/bin/sh
set -e
# Handles the substitution for the BACK_END_PREFIX variable in the index.html file
# See index.js for more details.
envsubst '${BACK_END_PREFIX}' \
  < /usr/share/nginx/html/index.html \
  > /tmp/index.html
mv /tmp/index.html /usr/share/nginx/html/index.html
exec nginx -g "daemon off;"
