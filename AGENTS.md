# AGENTS.md

This file provides guidance to agents when working with code in this repository.

## Build/Test Commands

```bash
cabal build                           # Build library and executable
cabal test                            # Run all tests
cabal test --test-option=--match="/safeHead/"  # Run single test by name pattern
./rebuild.sh                          # Full rebuild: clean, regenerate, build, test (sources .env)
./generate.sh                         # Regenerate OpenAI client from OpenAPI spec
```

## Non-Obvious Project Patterns

- **Generated code in src/OpenAI/Client/Generated/**: Auto-generated from `openapi/openai-chat.yaml` using `openapi3-code-generator-exe`. Never edit directly - modify spec and run `./generate.sh`
- **generate.sh patches aeson import**: Generated code uses `Data.Aeson.Decoding.eitherDecodeStrict` which doesn't exist - script patches to `Data.Aeson.eitherDecodeStrict`
- **rebuild.sh sources .env**: Contains `OPENAI_API_KEY` and other env vars needed for integration tests
- **hspec-discover**: Test discovery is automatic via `{-# OPTIONS_GHC -F -pgmF hspec-discover #-}` in `test/Spec.hs` - test modules must end in `Spec.hs` and export `spec :: Spec`
- **Integration tests skip gracefully**: OpenAI tests use `pendingWith` when `OPENAI_API_KEY` not set

## Code Style

- Haskell2010 with `-Wall`
- Haddock documentation with `{-| ... -}` module headers and `-- |` for functions
- Use `(..)` exports for types with all constructors/accessors
- Qualified imports for external libs (`qualified Data.Text as T`)
