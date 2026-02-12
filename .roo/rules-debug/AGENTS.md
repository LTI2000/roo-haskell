# Debug Mode Rules

## Common Issues

- **"Variable not in scope" for generated types**: Check if type is exported from `OpenAI.Client` - may need to add to export list
- **aeson decode errors in generated code**: The `generate.sh` script patches a known issue - if you see `Data.Aeson.Decoding` errors, re-run `./generate.sh`
- **Integration tests pending**: Set `OPENAI_API_KEY` env var or source `.env` file
- **Test not found**: Ensure test module exports `spec :: Spec` and filename ends in `Spec.hs`

## Debugging Commands

```bash
cabal test --test-show-details=direct  # See full test output
cabal repl roo-haskell                 # Interactive REPL with library loaded
cabal repl roo-haskell-test            # REPL with test dependencies
```
