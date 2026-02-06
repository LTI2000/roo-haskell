{-|
Module      : OpenAIClientSpec
Description : Integration tests for OpenAI.Client
Copyright   : (c) Roo, 2024
License     : MIT

Integration tests for the OpenAI-compatible chat completions client.
These tests require a valid API key and endpoint to run.

= Environment Variables

* @OPENAI_API_KEY@ - Required. The API key for authentication.
* @OPENAI_BASE_URL@ - Optional. The base URL for the API endpoint.
  Defaults to 'openAIBaseURL'.
* @OPENAI_MODEL@ - Optional. The model to use for testing.
  Defaults to "gpt-3.5-turbo".

= Running Tests

To run the integration tests:

@
OPENAI_API_KEY=sk-... cabal test --test-show-details=direct
@

For local endpoints like Ollama (use 'ollamaBaseURL'):

@
OPENAI_BASE_URL=http://localhost:11434 OPENAI_MODEL=llama2 cabal test
@
-}
module OpenAIClientSpec (spec) where

import Test.Hspec
import System.Environment (lookupEnv)
import Data.Maybe (fromMaybe)
import Data.List (isInfixOf)
import Data.Char (toLower)
import qualified Data.Text as T

import OpenAI.Client

-- | Get test configuration from environment variables
getTestConfig :: IO (Maybe ClientConfig)
getTestConfig = do
    maybeKey <- lookupEnv "OPENAI_API_KEY"
    maybeUrl <- lookupEnv "OPENAI_BASE_URL"
    return $ case maybeKey of
        Nothing -> Nothing
        Just apiKey ->
            let baseUrl = fromMaybe (T.unpack openAIBaseURL) maybeUrl
            in Just $ mkConfig baseUrl apiKey

-- | Get the model to use for testing
getTestModel :: IO String
getTestModel = do
    maybeModel <- lookupEnv "OPENAI_MODEL"
    return $ fromMaybe "gpt-3.5-turbo" maybeModel

spec :: Spec
spec = describe "OpenAI.Client" $ do
    describe "Configuration" $ do
        it "creates a valid configuration" $ do
            let config = mkConfig (T.unpack openAIBaseURL) "test-key"
            configBaseUrl config `shouldBe` T.unpack openAIBaseURL
            configApiKey config `shouldBe` "test-key"

    describe "Message helpers" $ do
        it "creates user messages correctly" $ do
            let msg = mkUserMessage "Hello"
            chatMessageContent msg `shouldBe` T.pack "Hello"
            chatMessageRole msg `shouldBe` ChatMessageRoleEnumUser

        it "creates system messages correctly" $ do
            let msg = mkSystemMessage "You are helpful"
            chatMessageContent msg `shouldBe` T.pack "You are helpful"
            chatMessageRole msg `shouldBe` ChatMessageRoleEnumSystem

        it "creates assistant messages correctly" $ do
            let msg = mkAssistantMessage "I can help"
            chatMessageContent msg `shouldBe` T.pack "I can help"
            chatMessageRole msg `shouldBe` ChatMessageRoleEnumAssistant

    describe "Request building" $ do
        it "creates a chat request with correct model" $ do
            let req = mkChatRequest "gpt-4" [mkUserMessage "test"]
            chatCompletionRequestModel req `shouldBe` T.pack "gpt-4"

        it "creates a chat request with messages" $ do
            let msgs = [mkSystemMessage "Be helpful", mkUserMessage "Hello"]
                req = mkChatRequest "gpt-3.5-turbo" msgs
            length (chatCompletionRequestMessages req) `shouldBe` 2

    describe "chatCompletion (integration)" $ do
        it "successfully completes a simple chat request" $ do
            maybeConfig <- getTestConfig
            case maybeConfig of
                Nothing -> pendingWith "OPENAI_API_KEY environment variable not set"
                Just config -> do
                    model <- getTestModel
                    result <- simpleChatCompletion config model "Say 'hello' and nothing else."
                    case result of
                        Left err -> expectationFailure $ "API call failed: " ++ err
                        Right response -> do
                            -- The response should contain some text
                            length response `shouldSatisfy` (> 0)
                            -- It should contain "hello" (case insensitive)
                            map toLower response `shouldSatisfy` 
                                (\r -> "hello" `isInfixOf` r)

        it "handles multi-turn conversations" $ do
            maybeConfig <- getTestConfig
            case maybeConfig of
                Nothing -> pendingWith "OPENAI_API_KEY environment variable not set"
                Just config -> do
                    model <- getTestModel
                    let messages = 
                            [ mkSystemMessage "You are a calculator. Only respond with numbers."
                            , mkUserMessage "What is 2 + 2?"
                            ]
                        request = mkChatRequest model messages
                    result <- chatCompletion config request
                    case result of
                        Left err -> expectationFailure $ "API call failed: " ++ err
                        Right response -> do
                            -- Should have at least one choice
                            chatCompletionResponseChoices response `shouldSatisfy` (not . null)

        it "returns error for invalid API key" $ do
            let config = mkConfig (T.unpack openAIBaseURL) "invalid-key"
            model <- getTestModel
            result <- simpleChatCompletion config model "Hello"
            case result of
                Left err -> err `shouldSatisfy` (\e -> "401" `isInfixOf` e || "Unauthorized" `isInfixOf` e)
                Right _ -> expectationFailure "Expected authentication error"
