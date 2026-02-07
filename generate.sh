rm -rf tmp

openapi3-code-generator-exe                \
  --specification openapi/openai-chat.yaml \
  --output-dir tmp                         \
  --module-name OpenAI.Generated           \
  --do-not-generate-stack-project

rm -rf src/OpenAI/Generated*

cp -r tmp/src .

rm -rf tmp

