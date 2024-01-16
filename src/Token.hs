{-
-- EPITECH PROJECT, 2023
-- Glados
-- File description:
-- Parse
-}
{-# LANGUAGE LambdaCase #-}

module Token (parseInput, printTokens, parseBoolConst, parseCharConst, parseStringConst, parseNullConst, parseConstant, parseComment, parseSpecialChar, parseSymbol, parseBinaryOperator, parseUnaryOperator, parseToken, parseFloatConst, parseIntConst) where

import Control.Applicative (Alternative (..))
import Data.Char (isAlphaNum)
import Data.Functor (($>))
import Parser (Parser (..), parseDouble, parseNumber, parseString, satisfy)
import Types (BinaryOperator (..), Constant (..), SpecialChar (..), Token (..), UnaryOperator (..))

stringValidChars :: [Char]
stringValidChars = ['#' .. '~'] ++ [' ', '!', '\n', '\t']

parseIntConst :: Parser Token
parseIntConst = Const . Int <$> parseNumber

parseFloatConst :: Parser Token
parseFloatConst = Const . Float <$> parseDouble

parseBoolConst :: Parser Token
parseBoolConst =
  parseString "True" <|> parseString "False" >>= \case
    "True" -> pure $ Const (Bool True)
    "False" -> pure $ Const (Bool False)
    _ -> empty

parseCharConst :: Parser Token
parseCharConst =
  parseString "'" *> satisfy (/= '\'') <* parseString "'" >>= \c ->
    pure $ Const (Char c)

parseStringConst :: Parser Token
parseStringConst =
  parseString "\"" *> many (satisfy (`elem` stringValidChars)) <* parseString "\"" >>= \s ->
    pure $ Const (String s)

parseNullConst :: Parser Token
parseNullConst = parseString "NULL" $> Const Null

parseConstant :: Parser Token
parseConstant =
  parseFloatConst
    <|> parseIntConst
    <|> parseBoolConst
    <|> parseNullConst
    <|> parseCharConst
    <|> parseStringConst

parseComment :: Parser Token
parseComment = (parseString "#" *> parseUntilNewline) >> pure (Special Comment)
  where
    parseUntilNewline :: Parser String
    parseUntilNewline = many (satisfy (/= '\n'))

parseSpecialChar :: Parser Token
parseSpecialChar =
  parseOneChar '(' OpenParenthesis
    <|> parseOneChar ')' CloseParenthesis
    <|> parseOneChar '[' OpenBracket
    <|> parseOneChar ']' CloseBracket
    <|> parseOneChar '{' OpenBrace
    <|> parseOneChar '}' CloseBrace
    <|> parseOneChar ',' Comma
    <|> parseOneChar ';' Semicolon
    <|> parseOneChar ':' Colon
    <|> parseOneChar '.' Dot
    <|> parseOneChar '?' QuestionMark
    <|> parseOneChar '!' ExclamationMark
    <|> parseOneChar '+' Plus
    <|> parseOneChar '-' Minus
    <|> parseOneChar '*' Asterisk
    <|> parseOneChar '/' Slash
    <|> parseOneChar ' ' Blank
    <|> parseOneChar '\n' Blank
    <|> parseOneChar '\t' Blank
    <|> parseComment
  where
    parseOneChar :: Char -> SpecialChar -> Parser Token
    parseOneChar c sc = parseString [c] $> Special sc

parseSymbol :: Parser Token
parseSymbol = Sym <$> some (satisfy isSymbolChar)
  where
    isSymbolChar :: Char -> Bool
    isSymbolChar c = isAlphaNum c || c == '_'

parseBinaryOperator :: Parser Token
parseBinaryOperator =
  parseString "=="
    $> BinaryOperator Equality
      <|> parseString "="
    $> BinaryOperator Equal
      <|> parseString "!="
    $> BinaryOperator NotEqual
      <|> parseString "<="
    $> BinaryOperator LessThanOrEqual
      <|> parseString ">="
    $> BinaryOperator GreaterThanOrEqual
      <|> parseString "<"
    $> BinaryOperator LessThan
      <|> parseString ">"
    $> BinaryOperator GreaterThan
      <|> parseString "&&"
    $> BinaryOperator And
      <|> parseString "||"
    $> BinaryOperator Or
      <|> parseString "+"
    $> BinaryOperator PlusOp
      <|> parseString "-"
    $> BinaryOperator MinusOp
      <|> parseString "*"
    $> BinaryOperator Multiply
      <|> parseString "/"
    $> BinaryOperator Divide
      <|> parseString "%"
    $> BinaryOperator Modulo

parseUnaryOperator :: Parser Token
parseUnaryOperator =
  parseString "!"
    $> UnaryOperator Not
      <|> parseString "-"
    $> UnaryOperator UnaryMinus

parseToken :: Parser Token
parseToken =
  parseConstant
    <|> parseBinaryOperator
    <|> parseUnaryOperator
    <|> parseSpecialChar
    <|> parseSymbol

parseInput :: String -> Either [Token] String
parseInput "" = Left []
parseInput str = case runParser parseToken str of
  Just (Special Blank, leftovers) -> parseInput leftovers
  Just (tokenHead, leftovers) -> case parseInput leftovers of
    Left tokenTail -> Left (tokenHead : tokenTail)
    Right err -> Right err
  Nothing -> Right str

printTokens :: [Token] -> IO ()
printTokens = mapM_ printToken
  where
    printToken :: Token -> IO ()
    printToken = print
