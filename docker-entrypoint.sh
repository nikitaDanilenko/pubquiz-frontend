#!/bin/sh
set -e
envsubst '${BACK_END_PREFIX}' \
  < /usr/share/nginx/html/index.html \
  > /tmp/index.html
mv /tmp/index.html /usr/share/nginx/html/index.html
exec nginx -g "daemon off;"
