# Architect Mode Rules

## Architecture Decisions

- **Generated code isolation**: `OpenAI.Client.Generated` is auto-generated and wrapped by `OpenAI.Client` - all user-facing API goes through the wrapper
- **OpenAPI-first**: API changes require modifying `openapi/openai-chat.yaml` and regenerating
- **Compatibility layer**: `OpenAI.Client` abstracts over OpenAI, Ollama, LM Studio, vLLM endpoints

## Extension Points

- Add new OpenAPI operations to `openapi/openai-chat.yaml`, regenerate, then wrap in `OpenAI.Client`
- Add new list utilities to `ListUtils.hs` with corresponding `ListUtilsSpec.hs` tests
- Integration tests use env vars for endpoint configuration - no hardcoded URLs
