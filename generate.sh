#!/bin/bash

set -e

rm -rf tmp

openapi3-code-generator-exe                \
  --specification openapi/openai-chat.yaml \
  --output-dir tmp                         \
  --module-name OpenAI.Client.Generated    \
  --do-not-generate-stack-project

# Fix build errors in CreateChatCompletion.hs:
# The generated code uses Data.Aeson.Decoding.eitherDecodeStrict
# which doesn't exist in the aeson library.
# Replace it with the correct function Data.Aeson.eitherDecodeStrict.
# Using a portable sed approach that works on both macOS and Linux
TARGET_FILE="tmp/src/OpenAI/Client/Generated/Operations/CreateChatCompletion.hs"
sed 's/Data\.Aeson\.Decoding\.eitherDecodeStrict/Data.Aeson.eitherDecodeStrict/g' \
  "$TARGET_FILE" > "$TARGET_FILE.tmp" && mv "$TARGET_FILE.tmp" "$TARGET_FILE"

rm -rf src/OpenAI/Client/Generated*

cp -r tmp/src .

rm -rf tmp

# Apply hlint refactoring to each generated Haskell file
echo "Applying hlint refactoring to generated files..."
find src/OpenAI/Client/Generated -name "*.hs" -type f | while read -r file; do
  echo "  Refactoring: $file"
  hlint --refactor --refactor-options="--inplace" "$file"
done
echo "HLint refactoring complete."
