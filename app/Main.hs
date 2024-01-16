{-
-- EPITECH PROJECT, 2023
-- Glados
-- File description:
-- Main
-}

module Main (main) where

import Arguments (Conf (..), Mode (..), displayHelp, parseArgs)
import CompilatorBytecode (compileToByteCode, toByteString)
import Compiler (concatenateInstructions)
import Data.Maybe (fromJust, fromMaybe)
import FileReader (concatenateAllContents, readFileSafe)
import FileWriter (saveToFile)
import System.Environment (getArgs)
import System.Exit (ExitCode (..), exitSuccess, exitWith)
import Token (parseInput)
import TokenToAst (transformTokensToAST)
import Types (Token)

main :: IO ()
main = do
  args <- getArgs
  maybeConf <- parseArgs args
  case maybeConf of
    Just conf ->
      ( if help conf
          then displayHelp >> exitSuccess
          else
            ( case lib conf of
                Just lib' -> do
                  result <- concatenateAllContents (fromMaybe [] (input conf)) lib'
                  startProcess conf result
                Nothing -> do
                  result <- readFileSafe (fromJust (input conf))
                  startProcess conf result
            )
      )
    Nothing -> displayHelp >> exitWith (ExitFailure 84)

startProcess :: Conf -> String -> IO ()
startProcess conf content =
  case mode conf of
    Just Binary -> processBinaryMode conf $ parseInput content
    Just Text -> processTextMode conf $ parseInput content
    Just Python -> processPythonMode conf $ parseInput content
    Nothing -> putStrLn "No mode specified." >> exitWith (ExitFailure 84)

processBinaryMode :: Conf -> Either [Token] String -> IO ()
processBinaryMode conf (Left tokens) =
  case transformTokensToAST tokens of
    Right asts -> saveToFile (fromJust (output conf)) $ toByteString $ compileToByteCode $ concatenateInstructions asts
    Left err -> putStrLn $ "AST Transformation failed: " ++ err
processBinaryMode _ (Right err) = putStrLn $ "Parsing failed. Remaining input: " ++ err

processTextMode :: Conf -> Either [Token] String -> IO ()
processTextMode conf (Left tokens) =
  case transformTokensToAST tokens of
    Right asts -> saveToFile (fromJust (output conf)) $ concatenateInstructions asts
    Left err -> putStrLn $ "AST Transformation failed: " ++ err
processTextMode _ (Right err) = putStrLn $ "Parsing failed. Remaining input: " ++ err

processPythonMode :: Conf -> Either [Token] String -> IO ()
processPythonMode _ (Left tokens) =
  case transformTokensToAST tokens of
    Right asts -> putStrLn $ "Python mode not implemented yet, asts: " ++ show asts -- TODO: Implement python mode
    Left err -> putStrLn $ "AST Transformation failed: " ++ err
processPythonMode _ (Right err) = putStrLn $ "Parsing failed. Remaining input: " ++ err
