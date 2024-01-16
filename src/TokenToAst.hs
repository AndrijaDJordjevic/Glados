{-
-- EPITECH PROJECT, 2023
-- Glados
-- File description:
-- TokenToAst
-}
{-# LANGUAGE LambdaCase #-}

module TokenToAst (transformTokensToAST, parseAssignment, parseIfStatement, parseWhileStatement, parseFunction, parseForStatement, parseReturnStatement, parseBlock, parseTokens, parseInstruction) where

import Types
  ( AST (..),
    BinaryOperator (..),
    SpecialChar (..),
    Token (..),
  )

transformTokensToAST :: [Token] -> Either String [AST]
transformTokensToAST tokens =
  case parseTokens tokens of
    Right (ast, []) -> Right ast
    Right (_, remainingTokens) -> Left $ "Unprocessed tokens: " ++ show remainingTokens
    Left errMsg -> Left errMsg

parseTokens :: [Token] -> Either String ([AST], [Token])
parseTokens [] = Right ([], [])
parseTokens ts =
  parseInstruction ts >>= \(ast, rest) ->
    parseTokens rest >>= \(restASTs, remaining) ->
      return (ast : restASTs, remaining)

parseInstruction :: [Token] -> Either String (AST, [Token])
parseInstruction (Sym "var" : rest) =
  parseAssignment rest >>= \(assignment, remaining) ->
    case remaining of
      (Special Semicolon) : restAfterSemicolon -> return (assignment, restAfterSemicolon)
      _ -> Left $ "Expected ';' after assignment\t" ++ show remaining
parseInstruction (Special Comment : rest) = parseInstruction rest
parseInstruction (Sym "if" : rest) = parseIfStatement rest
parseInstruction (Sym "while" : rest) = parseWhileStatement rest
parseInstruction (Sym "for" : rest) = parseForStatement rest
parseInstruction (Sym "return" : rest) = parseReturnStatement rest
parseInstruction (Sym "func" : rest) = parseFunction rest
parseInstruction (Sym "print" : Special OpenParenthesis : rest) =
  case parseExpression rest of
    Right (expr, Special CloseParenthesis : Special Semicolon : remainingTokens) ->
      Right (Print expr, remainingTokens)
    _ -> Left "Syntax error in print statement"
parseInstruction (Sym a : rest) =
  parseAssignment (Sym a : rest) >>= \(assigment, remaining) ->
    case remaining of
      (Special Semicolon) : restAfterSemicolon -> return (assigment, restAfterSemicolon)
      _ -> Left "Expected ';' after assignment"
parseInstruction err = Left $ "Invalid instruction:\t" ++ show err

parseAssignment :: [Token] -> Either String (AST, [Token])
parseAssignment (Sym name : Special OpenParenthesis : rest) =
  case splitOnToken (Special CloseParenthesis) rest of
    ([], Special CloseParenthesis : restAfterParenthesis) -> Right (FunctionCall name [], restAfterParenthesis)
    (args, Special CloseParenthesis : restAfterParenthesis) -> case getFunctionCallArgs args of
      Right args2 -> Right (FunctionCall name args2, restAfterParenthesis)
      Left err -> Left err
    _ -> Left "Expected ')' at the end of the function call"
parseAssignment (Sym name : BinaryOperator Equal : rest) =
  parseExpression rest >>= \(expr, remaining) -> Right (Binary Equal (Symbol name) expr, remaining)
parseAssignment tokens = Left $ "Invalid assignment:\t" ++ show tokens

parseExpression :: [Token] -> Either String (AST, [Token])
parseExpression = parseBinaryExpression

parseBinaryExpression :: [Token] -> Either String (AST, [Token])
parseBinaryExpression tokens =
  parseTerm tokens >>= \(left, rest1) ->
    parseBinaryOperators rest1 left

parseBinaryOperators :: [Token] -> AST -> Either String (AST, [Token])
parseBinaryOperators (BinaryOperator op : rest) left =
  parseTerm rest >>= \(right, rest1) ->
    parseBinaryOperators rest1 (Binary op left right)
parseBinaryOperators rest left = Right (left, rest)

parseTerm :: [Token] -> Either String (AST, [Token])
parseTerm tokens =
  parseFactor tokens >>= \(factor, rest1) ->
    parseTermOperators rest1 factor

parseTermOperators :: [Token] -> AST -> Either String (AST, [Token])
parseTermOperators (BinaryOperator op : rest) left =
  parseFactor rest >>= \(right, rest1) ->
    parseTermOperators rest1 (Binary op left right)
parseTermOperators rest left = Right (left, rest)

parseFactor :: [Token] -> Either String (AST, [Token])
parseFactor (Const c : rest) = Right (Constant c, rest)
parseFactor (Sym s : rest) = Right (Symbol s, rest)
parseFactor (Special OpenParenthesis : rest) =
  parseExpression rest >>= \(expr, rest1) ->
    case rest1 of
      (Special CloseParenthesis : rest2) -> Right (expr, rest2)
      _ -> Left "Mismatched parentheses in expression"
parseFactor _ = Left "Invalid factor in expression"

parseIfStatement :: [Token] -> Either String (AST, [Token])
parseIfStatement tokens = case parseConditionAndBlock tokens of
  Right (condition, block, rest) -> case rest of
    (Sym "else" : rest2) ->
      parseElseStatement rest2 >>= \(elsePart, restAfterElse) ->
        return (IfElse condition block elsePart, restAfterElse)
    _ -> return (IfElse condition block (Block []), rest)
  Left err -> Left err

parseElseStatement :: [Token] -> Either String (AST, [Token])
parseElseStatement (Sym "if" : rest) = parseIfStatement rest
parseElseStatement tokens =
  parseBlock tokens

parseCondition :: [Token] -> Either String (AST, [Token])
parseCondition (Special OpenParenthesis : tokens) =
  parseExpression tokens >>= \(expr1, rest1) ->
    case rest1 of
      (Special CloseParenthesis : rest2) -> Right (expr1, rest2)
      _ -> Left $ "parseCondition: " ++ show rest1
parseCondition t = Left $ "Invalid condition: " ++ show t

parseConditionAndBlock :: [Token] -> Either String (AST, AST, [Token])
parseConditionAndBlock tokens = case parseCondition tokens of
  Right (condition, rest) -> case parseBlock rest of
    Right (block, restAfterBlock) -> Right (condition, block, restAfterBlock)
    Left err -> Left err
  Left err -> Left err

parseBlock :: [Token] -> Either String (AST, [Token])
parseBlock tokens =
  case tokens of
    (Special OpenBrace : rest) ->
      case parseStatements rest of
        Left err -> Left err
        Right (statements, remaining) -> Right (Block statements, remaining)
    _ -> Left "Expected '{' at the beginning of the block"

parseWhileStatement :: [Token] -> Either String (AST, [Token])
parseWhileStatement tokens = case parseConditionAndBlock tokens of
  Right (condition, block, rest) -> Right (While condition block, rest)
  Left err -> Left err

parseFunction :: [Token] -> Either String (AST, [Token])
parseFunction (Sym name : Special OpenParenthesis : rest) =
  case splitOnToken (Special CloseParenthesis) rest of
    ([], Special CloseParenthesis : restAfterParenthesis) -> parseBlock restAfterParenthesis >>= \(block, remaining) -> Right (FunctionDefinition name [] block, remaining)
    (args, Special CloseParenthesis : restAfterParenthesis) -> case getFonctionArgs args of
      Right args2 -> parseBlock restAfterParenthesis >>= \(block, remaining) -> Right (FunctionDefinition name args2 block, remaining)
      Left err -> Left err
    _ -> Left "Expected ')' at the end of the function definition"
parseFunction _ = Left "Invalid function definition"

parseForStatement :: [Token] -> Either String (AST, [Token])
parseForStatement (Special OpenParenthesis : tokens) = case parseExpression tokens of
  Left err -> Left err
  Right (initialisation, Special Semicolon : rest) -> case parseCondition (Special OpenParenthesis : rest) of
    Left err -> Left err
    Right (condition, rest2) -> case parseBlock rest2 of
      Right (block, rest3) -> Right (For initialisation condition block, rest3)
      _ -> Left "Expected '}' at the end of the block"
  _ -> Left "Expected ';' after initialisation"
parseForStatement _ = Left "for"

parseReturnStatement :: [Token] -> Either String (AST, [Token])
parseReturnStatement tokens = case parseExpression tokens of
  Left err -> Left err
  Right (expr, rest) -> case rest of
    (Special Semicolon : rest2) -> Right (Return expr, rest2)
    _ -> Left "Expected ';' after return expression"

parseStatements :: [Token] -> Either String ([AST], [Token])
parseStatements tokens = parseStatements' tokens []
  where
    parseStatements' :: [Token] -> [AST] -> Either String ([AST], [Token])
    parseStatements' [] _ = Left "Expected '}' at the end of the block"
    parseStatements' (Special CloseBrace : rest) acc = Right (reverse acc, rest)
    parseStatements' ts acc = case parseInstruction ts of
      Left err -> Left err
      Right (ast, rest) -> parseStatements' rest (ast : acc)

splitOnToken :: Token -> [Token] -> ([Token], [Token])
splitOnToken t = break (== t)

getFonctionArgs :: [Token] -> Either String [String]
getFonctionArgs ts =
  if all
    ( \case
        Sym _ -> True
        _ -> False
    )
    ts
    then Right $ map (\case Sym s -> s; _ -> error "Pattern match failure") ts
    else Left "Not all elements are Sym tokens"

getFunctionCallArgs :: [Token] -> Either String [AST]
getFunctionCallArgs [] = Right []
getFunctionCallArgs a =
  case parseExpression a of
    Right (expr, remaining) ->
      case remaining of
        Special Comma : rest' -> (expr :) <$> getFunctionCallArgs rest'
        [] -> Right [expr]
        _ -> Left "Invalid function call arguments"
    Left err -> Left $ "y'a un truc:  " ++ err
