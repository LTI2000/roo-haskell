# Ask Mode Rules

## Project Structure

- `src/ListUtils.hs` - Safe list operations (safeHead, safeLast, etc.)
- `src/OpenAI/Client.hs` - High-level wrapper around generated OpenAI client
- `src/OpenAI/Client/Generated/` - Auto-generated from OpenAPI spec (don't reference for patterns)
- `openapi/openai-chat.yaml` - OpenAPI spec for chat completions API
- `test/*Spec.hs` - HSpec tests with QuickCheck property tests

## Key APIs

- `OpenAI.Client`: Use `simpleChatCompletion` for single-turn, `chatCompletion` with `mkChatRequest` for multi-turn
- `ListUtils`: All functions are total (no partial functions) - return `Maybe` for potentially failing operations
