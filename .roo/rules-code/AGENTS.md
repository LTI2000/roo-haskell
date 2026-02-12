# Code Mode Rules

## Critical Patterns

- **Never edit files in `src/OpenAI/Client/Generated/`** - regenerate with `./generate.sh` instead
- **Test modules must export `spec :: Spec`** and filename must end in `Spec.hs` for hspec-discover
- **Use `mkUserMessage`, `mkSystemMessage`, `mkAssistantMessage`** from `OpenAI.Client` - don't construct `ChatMessage` directly
- **`mkChatRequest` requires non-empty message list** - will error at runtime on empty list
- **Access choice message via `choiceMessage`** field accessor, not a separate function

## Testing Integration Tests Locally

```bash
source .env  # Load OPENAI_API_KEY, OPENAI_BASE_URL, OPENAI_MODEL
cabal test --test-show-details=direct
```
