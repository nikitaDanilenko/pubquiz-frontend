ARG BUILD_PLATFORM=linux/amd64
FROM --platform=$BUILD_PLATFORM node:22-alpine AS build

WORKDIR /app

# The setting is necessary for 'create-elm-app'; outdated, but 'create-elm-app' is not updated (anymore).
ENV NODE_OPTIONS=--openssl-legacy-provider

RUN npm install -g create-elm-app

COPY elm.json elmapp.config.js ./
COPY src/ src/
COPY public/ public/

RUN elm-app build

FROM nginx:alpine

COPY --from=build /app/build /usr/share/nginx/html
COPY nginx.conf /etc/nginx/conf.d/default.conf
