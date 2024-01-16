{-
-- EPITECH PROJECT, 2023
-- Glados
-- File description:
-- Parser
-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}

module Parser
  ( Parser (..),
    parseOr,
    parseAnd,
    parseChar,
    parseNumber,
    parseDouble,
    parseIntToString,
    parseString,
    satisfy,
  )
where

import Control.Applicative (Alternative (..), optional)
import Control.Monad ((>=>))
import Data.Char (isDigit)
import Data.List (stripPrefix)

newtype Parser a = Parser {runParser :: String -> Maybe (a, String)}

instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap f (Parser p) = Parser (p >=> (\(x, rest) -> Just (f x, rest)))

instance Applicative Parser where
  pure :: a -> Parser a
  pure x = Parser $ \input -> Just (x, input)

  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  (Parser pf) <*> px =
    Parser
      ( pf
          >=> ( \(f, rest1) ->
                  runParser px rest1 >>= \(x, rest2) -> Just (f x, rest2)
              )
      )

instance Monad Parser where
  return :: a -> Parser a
  return = pure

  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  (Parser p) >>= f = Parser (p >=> (\(x, rest) -> runParser (f x) rest))

instance Alternative Parser where
  empty :: Parser a
  empty = Parser $ const Nothing

  (<|>) :: Parser a -> Parser a -> Parser a
  (<|>) = parseOr

parseOr :: Parser a -> Parser a -> Parser a
parseOr (Parser p1) (Parser p2) = Parser $
  \str -> case p1 str of
    Just x -> Just x
    Nothing -> p2 str

parseAnd :: Parser a -> Parser b -> Parser (a, b)
parseAnd (Parser p1) (Parser p2) = Parser $
  \str -> case p1 str of
    Just (x, rest1) -> case p2 rest1 of
      Just (y, rest2) -> Just ((x, y), rest2)
      Nothing -> Nothing
    Nothing -> Nothing

parseChar :: Parser Char
parseChar = Parser $ \case
  (c : rest) -> Just (c, rest)
  _ -> Nothing

parseOneChar :: Char -> Parser Char
parseOneChar c = satisfy (== c)

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser $ \case
  (c : rest) | p c -> Just (c, rest)
  _ -> Nothing

parseIntToString :: Parser String
parseIntToString = some (satisfy isDigit)

parseNumber :: Parser Integer
parseNumber =
  optional (parseOneChar '-')
    >>= \sign ->
      some (satisfy isDigit)
        >>= \digits ->
          let number = read (maybe "" (: []) sign ++ digits)
           in Parser $ \input -> Just (number, input)

parseDouble :: Parser Double
parseDouble =
  (\maybeSign intPart fracPart -> read (maybe "" (: []) maybeSign ++ intPart ++ maybe "" ('.' :) fracPart) :: Double)
    <$> optional (parseOneChar '-')
    <*> parseIntToString
    <*> (parseOneChar '.' *> optional parseIntToString)

parseString :: String -> Parser String
parseString strToMatch = Parser $
  \str -> case stripPrefix strToMatch str of
    Just newStr -> Just (strToMatch, newStr)
    Nothing -> Nothing
