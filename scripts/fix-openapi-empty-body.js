// Post-processing fixes for elm-open-api
// See: https://github.com/wolfadex/elm-open-api-cli/issues/302

const fs = require("fs");

// Fix 1: Empty body handling for 204 responses
const commonFile = "src/Generated/OpenApi/Common.elm";

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

let commonContent = fs.readFileSync(commonFile, "utf8");
commonContent = commonContent.replaceAll(original, fixed);
fs.writeFileSync(commonFile, commonContent);
