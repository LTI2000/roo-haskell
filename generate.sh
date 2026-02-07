rm -rf tmp

openapi3-code-generator-exe                \
  --specification openapi/openai-chat.yaml \
  --output-dir tmp                         \
  --module-name OpenAI.Generated           \
  --do-not-generate-stack-project

# Fix build errors in CreateChatCompletion.hs:
# The generated code uses Data.Aeson.Decoding.eitherDecodeStrict
# which doesn't exist in the aeson library.
# Replace it with the correct function Data.Aeson.eitherDecodeStrict.
# Using a portable sed approach that works on both macOS and Linux
TARGET_FILE="tmp/src/OpenAI/Generated/Operations/CreateChatCompletion.hs"
sed 's/Data\.Aeson\.Decoding\.eitherDecodeStrict/Data.Aeson.eitherDecodeStrict/g' \
  "$TARGET_FILE" > "$TARGET_FILE.tmp" && mv "$TARGET_FILE.tmp" "$TARGET_FILE"

rm -rf src/OpenAI/Generated*

cp -r tmp/src .

rm -rf tmp
