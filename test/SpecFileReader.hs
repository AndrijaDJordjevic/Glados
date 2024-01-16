{-
-- EPITECH PROJECT, 2023
-- Glados
-- File description:
-- FileReaderTest
-}
{-# LANGUAGE ScopedTypeVariables #-}

module SpecFileReader (fileReaderTests) where

import Control.Exception (IOException, catch)
import FileReader (readMyFile)
import Test.HUnit

-- Test cases
testNonexistentFile :: Test
testNonexistentFile = TestCase $ do
  result <- readMyFile "nonexistentfile.txt"
  assertEqual "Should return Nothing for non-existent file" Nothing result

testExistingFile :: Test
testExistingFile = TestCase $ do
  result <- readMyFile "examples/foo.scm"
  assertEqual "Should return Just content for an existing file" (Just "(define foo 21)\n(* foo 2)") result

testExceptionHandling :: Test
testExceptionHandling = TestCase $ do
  result <- catch (readMyFile "/nonexistentfile.txt" >> pure Nothing) $ \(e :: IOException) -> pure (Just e)
  assertBool "Should handle exceptions and return Nothing" (result == Nothing)

-- Test suite
fileReaderTests :: Test
fileReaderTests =
  TestList
    [ TestLabel "NonexistentFile" testNonexistentFile,
      TestLabel "ExistingFile" testExistingFile,
      TestLabel "ExceptionHandling" testExceptionHandling
    ]
