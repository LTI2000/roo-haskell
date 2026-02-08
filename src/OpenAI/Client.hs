{-|
Module      : OpenAI.Client
Description : High-level client for OpenAI-compatible chat completions API
Copyright   : (c) Roo, 2024
License     : MIT
Maintainer  : roo@example.com
Stability   : experimental

This module provides a simplified interface for making chat completion
requests to OpenAI-compatible endpoints. It wraps the generated API code
with a more ergonomic interface.

= Usage Example

@
import OpenAI.Client

main :: IO ()
main = do
    let config = mkConfig "https://api.openai.com" "your-api-key"
    result <- simpleChatCompletion config "gpt-3.5-turbo" "Hello, how are you?"
    case result of
        Left err -> putStrLn $ "Error: " ++ err
        Right response -> putStrLn $ "Response: " ++ response
@

= Compatibility

This client is designed to work with any OpenAI-compatible endpoint:

* OpenAI API (https://api.openai.com)
* Ollama (http://localhost:11434)
* LM Studio (http://localhost:1234)
* vLLM and other compatible servers
-}
module OpenAI.Client
  ( -- * Configuration
    ClientConfig(..)
  , mkConfig
    -- * Chat Completions
  , chatCompletion
  , simpleChatCompletion
    -- * Types (re-exported from generated code)
  , ChatMessage(..)
  , ChatMessageRole(..)
  , ChatCompletionRequest(..)
  , ChatCompletionResponse(..)
  , Choice(..)
  , Usage(..)
    -- * Helper functions
  , mkUserMessage
  , mkSystemMessage
  , mkAssistantMessage
  , mkChatRequest
  ) where

import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import qualified Network.HTTP.Client as HTTP

import OpenAI.Client.Generated
  ( ChatMessage(..)
  , ChatMessageRole(..)
  , ChatCompletionRequest(..)
  , ChatCompletionResponse(..)
  , Choice(..)
  , Usage(..)
  , Configuration(..)
  , bearerAuthenticationSecurityScheme
  , createChatCompletionWithConfiguration
  , CreateChatCompletionResponse(..)
  , mkChatCompletionRequest
  , mkChatMessage
  )

-- | Configuration for the OpenAI client
data ClientConfig = ClientConfig
  { configBaseUrl   :: String    -- ^ Base URL (e.g., "https://api.openai.com")
  , configApiKey    :: String    -- ^ API key for authentication
  , configModelName :: String    -- ^ Model name (e.g., "gpt-4", "gpt-3.5-turbo")
  } deriving (Show, Eq)

-- | Create a client configuration with the given base URL, API key, and model name
--
-- ==== Examples
--
-- >>> let config = mkConfig "https://api.openai.com" "sk-..." "gpt-4"
-- >>> configBaseUrl config
-- "https://api.openai.com"
mkConfig :: String  -- ^ Base URL
         -> String  -- ^ API key
         -> String  -- ^ Model name
         -> ClientConfig
mkConfig = ClientConfig

-- | Create a user message
--
-- ==== Examples
--
-- >>> mkUserMessage "Hello, world!"
-- ChatMessage {chatMessageContent = "Hello, world!", chatMessageRole = ChatMessageRoleEnumUser}
mkUserMessage :: String -> ChatMessage
mkUserMessage content = mkChatMessage (T.pack content) ChatMessageRoleEnumUser

-- | Create a system message
--
-- ==== Examples
--
-- >>> mkSystemMessage "You are a helpful assistant."
-- ChatMessage {chatMessageContent = "You are a helpful assistant.", chatMessageRole = ChatMessageRoleEnumSystem}
mkSystemMessage :: String -> ChatMessage
mkSystemMessage content = mkChatMessage (T.pack content) ChatMessageRoleEnumSystem

-- | Create an assistant message
--
-- ==== Examples
--
-- >>> mkAssistantMessage "I'm here to help!"
-- ChatMessage {chatMessageContent = "I'm here to help!", chatMessageRole = ChatMessageRoleEnumAssistant}
mkAssistantMessage :: String -> ChatMessage
mkAssistantMessage content = mkChatMessage (T.pack content) ChatMessageRoleEnumAssistant

-- | Create a chat completion request with the given model and messages
--
-- Note: The messages list must be non-empty. If an empty list is provided,
-- this function will error at runtime.
--
-- ==== Examples
--
-- >>> let req = mkChatRequest "gpt-3.5-turbo" [mkUserMessage "Hello!"]
-- >>> chatCompletionRequestModel req
-- "gpt-3.5-turbo"
mkChatRequest :: String        -- ^ Model name
              -> [ChatMessage] -- ^ Messages (must be non-empty)
              -> ChatCompletionRequest
mkChatRequest model messages = 
    case NE.nonEmpty messages of
        Nothing -> error "mkChatRequest: messages list cannot be empty"
        Just neMessages -> mkChatCompletionRequest neMessages (T.pack model)

-- | Convert ClientConfig to the generated Configuration type
toConfiguration :: ClientConfig -> Configuration
toConfiguration cfg = Configuration
  { configBaseURL = T.pack (configBaseUrl cfg)
  , configSecurityScheme = bearerAuthenticationSecurityScheme (T.pack $ configApiKey cfg)
  , configIncludeUserAgent = True
  , configApplicationName = T.pack "roo-haskell-openai-client"
  }

-- | Send a chat completion request
--
-- This function sends a chat completion request to the configured endpoint
-- and returns either an error message or the full response.
--
-- ==== Examples
--
-- @
-- let config = mkConfig "https://api.openai.com" "sk-..."
--     request = mkChatRequest "gpt-3.5-turbo" [mkUserMessage "Hello!"]
-- result <- chatCompletion config request
-- case result of
--     Left err -> putStrLn $ "Error: " ++ err
--     Right response -> print response
-- @
chatCompletion :: ClientConfig
               -> ChatCompletionRequest
               -> IO (Either String ChatCompletionResponse)
chatCompletion cfg request = do
    let config = toConfiguration cfg
    response <- createChatCompletionWithConfiguration config request
    let body = HTTP.responseBody response
    return $ case body of
        CreateChatCompletionResponse200 resp -> Right resp
        CreateChatCompletionResponse400 -> Left "Bad request (400)"
        CreateChatCompletionResponse401 -> Left "Unauthorized - Invalid or missing API key (401)"
        CreateChatCompletionResponse429 -> Left "Rate limit exceeded (429)"
        CreateChatCompletionResponse500 -> Left "Internal server error (500)"
        CreateChatCompletionResponseError err -> Left $ "Error: " ++ err

-- | Simplified chat completion with just model and user message
--
-- This is a convenience function for simple single-turn conversations.
-- It sends a single user message and returns the assistant's response content.
--
-- ==== Examples
--
-- @
-- let config = mkConfig "https://api.openai.com" "sk-..."
-- result <- simpleChatCompletion config "gpt-3.5-turbo" "What is 2+2?"
-- case result of
--     Left err -> putStrLn $ "Error: " ++ err
--     Right answer -> putStrLn $ "Answer: " ++ answer
-- @
simpleChatCompletion :: ClientConfig
                     -> String  -- ^ Model name
                     -> String  -- ^ User message
                     -> IO (Either String String)  -- ^ Response content or error
simpleChatCompletion cfg model userMessage = do
    let request = mkChatRequest model [mkUserMessage userMessage]
    result <- chatCompletion cfg request
    return $ case result of
        Left err -> Left err
        Right response ->
            case chatCompletionResponseChoices response of
                [] -> Left "No choices in response"
                (choice:_) ->
                    let msg = choiceMessage choice
                    in Right $ T.unpack $ chatMessageContent msg
