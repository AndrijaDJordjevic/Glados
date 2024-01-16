{-
-- EPITECH PROJECT, 2023
-- Glados
-- File description:
-- FileReader
-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module FileReader (concatenateFilesContent, readMyFile, concatenateAllContents, readFileSafe) where

import Control.Exception (IOException, SomeException, try)
import Control.Monad (filterM)
import System.Directory (doesDirectoryExist, listDirectory)
import System.Exit (ExitCode (..), exitWith)
import System.FilePath ((</>))
import System.IO ()

readMyFile :: FilePath -> IO (Maybe String)
readMyFile filename =
  try (readFile filename) >>= \case
    Left (_ :: SomeException) -> pure Nothing
    Right content -> pure $ Just content

isFile :: FilePath -> IO Bool
isFile path = do
  result <- try (doesDirectoryExist path)
  case result of
    Left (e :: IOException) -> do
      putStrLn $ "Error checking file '" ++ path ++ "': " ++ show e
      exitWith (ExitFailure 84)
    Right isDir -> return (not isDir)

readFileSafe :: FilePath -> IO String
readFileSafe path = do
  result <- try (readFile path)
  case result of
    Left (e :: IOException) -> do
      putStrLn $ "Error reading file '" ++ path ++ "': " ++ show e
      exitWith (ExitFailure 84)
    Right content -> return content

concatenateFilesContent :: FilePath -> IO String
concatenateFilesContent dirPath = do
  result <- try (listDirectory dirPath)
  case result of
    Left (e :: IOException) -> do
      putStrLn $ "Error reading directory '" ++ dirPath ++ "': " ++ show e
      exitWith (ExitFailure 84)
    Right files -> do
      let fullPaths = map (dirPath </>) files
      fileContents <- mapM readFileSafe =<< filterM isFile fullPaths
      return $ concat fileContents

concatenateAllContents :: FilePath -> FilePath -> IO String
concatenateAllContents sourceFile dirPath = do
  dirContents <- concatenateFilesContent dirPath
  sourceFileContent <- readFileSafe sourceFile
  return $ dirContents ++ "\n" ++ sourceFileContent
