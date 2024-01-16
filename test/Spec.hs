{-
-- EPITECH PROJECT, 2023
-- Glados
-- File description:
-- Spec
-}

import SpecArguments (argumentsTests)
import SpecFileReader (fileReaderTests)
import SpecInterpreter (interpreterTests)
import SpecParser (parserTests)
import SpecToken (tokenTests)
import SpecTokenToAst (tokenToAstTests)
-- import SpecCompiler (compilerTests)
import Test.HUnit

main :: IO ()
main = do
  putStrLn "Running tests for FileReader..."
  _ <- runTestTT fileReaderTests
  putStrLn "----------------------------------"

  putStrLn "Running tests for Arguments..."
  _ <- runTestTT argumentsTests
  putStrLn "----------------------------------"
  putStrLn "Running tests for Interpreter..."
  _ <- runTestTT interpreterTests
  putStrLn "Running tests for Parser..."
  _ <- runTestTT parserTests
  putStrLn "----------------------------------"

  putStrLn "Running tests for Token..."
  _ <- runTestTT tokenTests
  putStrLn "----------------------------------"

  putStrLn "Running tests for TokenToAst..."
  _ <- runTestTT tokenToAstTests
  putStrLn "----------------------------------"

-- putStrLn "Running tests for Compiler..."
-- _ <- runTestTT compilerTests
-- putStrLn "Running tests for Binary Compiler..."
-- _ <- runTestTT compilerBinaryTests
-- putStrLn "----------------------------------"
