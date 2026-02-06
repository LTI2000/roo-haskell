# roo-haskell

A Haskell library providing list utilities and an OpenAI-compatible chat completions API client.

## Overview

This project provides:

- **List Utilities**: Safe list operations and common list manipulation functions
- **OpenAI Client**: A type-safe client for OpenAI-compatible chat completion APIs

Key features:

- Safe functional programming patterns (avoiding partial functions)
- Property-based testing with QuickCheck
- Unit testing with HSpec
- OpenAPI-generated type-safe API client
- Support for any OpenAI-compatible endpoint (OpenAI, Ollama, LM Studio, vLLM, etc.)

## Installation

### Prerequisites

- GHC (Glasgow Haskell Compiler) >= 8.10
- Cabal >= 3.0

### Building

```bash
# Build the library and executable
cabal build

# Run the demo executable
cabal run roo-haskell-exe

# Run the test suite
cabal test

# Run tests with verbose output
cabal test --test-show-details=direct
```

## OpenAI Client

The `OpenAI.Client` module provides a high-level interface for making chat completion requests to OpenAI-compatible endpoints.

### Quick Start

```haskell
import OpenAI.Client

main :: IO ()
main = do
    -- Create a configuration with your API endpoint and key
    let config = mkConfig "https://api.openai.com" "your-api-key"
    
    -- Make a simple chat completion request
    result <- simpleChatCompletion config "gpt-3.5-turbo" "Hello, how are you?"
    
    case result of
        Left err -> putStrLn $ "Error: " ++ err
        Right response -> putStrLn $ "Response: " ++ response
```

### Configuration

Create a client configuration using `mkConfig`:

```haskell
-- For OpenAI
let config = mkConfig "https://api.openai.com" "sk-your-api-key"

-- For Ollama (local)
let config = mkConfig "http://localhost:11434" ""

-- For LM Studio (local)
let config = mkConfig "http://localhost:1234" ""
```

### Creating Messages

Helper functions for creating chat messages:

```haskell
-- Create a user message
let userMsg = mkUserMessage "What is the capital of France?"

-- Create a system message
let systemMsg = mkSystemMessage "You are a helpful geography assistant."

-- Create an assistant message (for conversation history)
let assistantMsg = mkAssistantMessage "The capital of France is Paris."
```

### Multi-turn Conversations

For more complex conversations, use `chatCompletion` with a custom request:

```haskell
import OpenAI.Client

main :: IO ()
main = do
    let config = mkConfig "https://api.openai.com" "your-api-key"
        messages = 
            [ mkSystemMessage "You are a helpful math tutor."
            , mkUserMessage "What is 2 + 2?"
            ]
        request = mkChatRequest "gpt-3.5-turbo" messages
    
    result <- chatCompletion config request
    
    case result of
        Left err -> putStrLn $ "Error: " ++ err
        Right response -> do
            let choices = chatCompletionResponseChoices response
            case choices of
                [] -> putStrLn "No response"
                (choice:_) -> do
                    let msg = choiceMessage choice
                    putStrLn $ "Response: " ++ T.unpack (chatMessageContent msg)
```

### API Reference

#### Configuration

| Function | Type | Description |
|----------|------|-------------|
| `mkConfig` | `String -> String -> ClientConfig` | Create a client configuration |
| `configBaseUrl` | `ClientConfig -> String` | Get the base URL |
| `configApiKey` | `ClientConfig -> String` | Get the API key |

#### Chat Completions

| Function | Type | Description |
|----------|------|-------------|
| `simpleChatCompletion` | `ClientConfig -> String -> String -> IO (Either String String)` | Simple single-turn chat |
| `chatCompletion` | `ClientConfig -> ChatCompletionRequest -> IO (Either String ChatCompletionResponse)` | Full chat completion |
| `mkChatRequest` | `String -> [ChatMessage] -> ChatCompletionRequest` | Create a chat request |

#### Message Helpers

| Function | Type | Description |
|----------|------|-------------|
| `mkUserMessage` | `String -> ChatMessage` | Create a user message |
| `mkSystemMessage` | `String -> ChatMessage` | Create a system message |
| `mkAssistantMessage` | `String -> ChatMessage` | Create an assistant message |

### Running Integration Tests

The integration tests require a valid API key:

```bash
# With OpenAI
OPENAI_API_KEY=sk-... cabal test --test-show-details=direct

# With Ollama (local)
OPENAI_BASE_URL=http://localhost:11434 OPENAI_MODEL=llama2 cabal test

# With LM Studio (local)
OPENAI_BASE_URL=http://localhost:1234 OPENAI_MODEL=local-model cabal test
```

Environment variables:
- `OPENAI_API_KEY` - Required for integration tests
- `OPENAI_BASE_URL` - Optional, defaults to `https://api.openai.com`
- `OPENAI_MODEL` - Optional, defaults to `gpt-3.5-turbo`

## List Utility Functions

### Safe Accessors

#### `safeHead :: [a] -> Maybe a`

Safe version of `head` that returns `Nothing` for empty lists.

```haskell
>>> safeHead [1, 2, 3]
Just 1

>>> safeHead []
Nothing
```

#### `safeLast :: [a] -> Maybe a`

Safe version of `last` that returns `Nothing` for empty lists.

```haskell
>>> safeLast [1, 2, 3]
Just 3

>>> safeLast []
Nothing
```

### Filtering

#### `filterBy :: (a -> Bool) -> [a] -> [a]`

Filter a list using a predicate function.

```haskell
>>> filterBy even [1, 2, 3, 4, 5, 6]
[2, 4, 6]

>>> filterBy (> 0) [-2, -1, 0, 1, 2]
[1, 2]
```

### Folding

#### `sumList :: Num a => [a] -> a`

Sum all elements in a list using a fold.

```haskell
>>> sumList [1, 2, 3, 4, 5]
15

>>> sumList []
0
```

#### `reverseList :: [a] -> [a]`

Reverse a list using a fold.

```haskell
>>> reverseList [1, 2, 3]
[3, 2, 1]

>>> reverseList "hello"
"olleh"
```

### Mapping

#### `mapDouble :: Num a => [a] -> [a]`

Double each element in a numeric list.

```haskell
>>> mapDouble [1, 2, 3]
[2, 4, 6]

>>> mapDouble [-1, 0, 1]
[-2, 0, 2]
```

## Testing

The test suite includes:

- **Unit tests** for list utilities and OpenAI client
- **Property-based tests** using QuickCheck
- **Integration tests** for the OpenAI client (requires API key)

### Running Tests

```bash
# Run all tests
cabal test

# Run with detailed output
cabal test --test-show-details=direct

# Run with QuickCheck verbose mode (shows generated test cases)
cabal test --test-option=--qc-max-success=1000
```

### Test Properties

Key properties verified by QuickCheck:

| Function | Property |
|----------|----------|
| `safeHead` | Non-empty list returns `Just (head xs)` |
| `safeLast` | `safeHead xs == safeLast (reverseList xs)` |
| `filterBy` | All elements in result satisfy predicate |
| `sumList` | `sumList (xs ++ ys) == sumList xs + sumList ys` |
| `reverseList` | Reversing twice gives original list |
| `mapDouble` | `sumList (mapDouble xs) == 2 * sumList xs` |

## Project Structure

```
roo-haskell/
├── roo-haskell.cabal          # Cabal build configuration
├── README.md                  # This file
├── openapi/
│   └── openai-chat.yaml       # OpenAPI specification for chat completions
├── src/
│   ├── ListUtils.hs           # List utility functions
│   └── OpenAI/
│       ├── Client.hs          # High-level OpenAI client
│       └── Generated/         # Generated API code from OpenAPI spec
├── app/
│   └── Main.hs                # Demo executable
├── test/
│   ├── Spec.hs                # hspec-discover entry point
│   ├── ListUtilsSpec.hs       # List utilities test suite
│   └── OpenAIClientSpec.hs    # OpenAI client test suite
└── plans/
    └── *.md                   # Development plans
```

## OpenAPI Specification

The OpenAI client is generated from an OpenAPI 3.0 specification located at `openapi/openai-chat.yaml`. This spec defines the `/v1/chat/completions` endpoint and is compatible with:

- OpenAI API
- Ollama
- LM Studio
- vLLM
- Any other OpenAI-compatible endpoint

To regenerate the client code after modifying the spec:

```bash
openapi3-code-generator-exe \
  --specification openapi/openai-chat.yaml \
  --output-dir src/OpenAI/Generated \
  --module-name OpenAI.Generated \
  --do-not-generate-stack-project \
  --force \
  --convert-to-camel-case
```

## License

MIT License
