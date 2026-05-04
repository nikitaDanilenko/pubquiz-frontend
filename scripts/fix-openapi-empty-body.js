// Post-processing fix for elm-open-api empty body bug
// See: https://github.com/wolfadex/elm-open-api/issues/14

const fs = require("fs");

const file = "src/Generated/OpenApi/Common.elm";

const original = `Http.GoodStatus_ httpMetadata body ->
                    case Json.Decode.decodeString successDecoder body of
                        Result.Ok value ->
                            Result.Ok value

                        Result.Err error ->
                            Result.Err (BadBody httpMetadata body)`;

const fixed = `Http.GoodStatus_ httpMetadata body ->
                    if httpMetadata.statusCode == 204 then
                        Json.Decode.decodeString successDecoder "null"
                            |> Result.mapError (\\_ -> BadBody httpMetadata body)

                    else
                        case Json.Decode.decodeString successDecoder body of
                            Result.Ok value ->
                                Result.Ok value

                            Result.Err error ->
                                Result.Err (BadBody httpMetadata body)`;

let content = fs.readFileSync(file, "utf8");
content = content.replaceAll(original, fixed);
fs.writeFileSync(file, content);
