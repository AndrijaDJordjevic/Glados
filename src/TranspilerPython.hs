{-# LANGUAGE LambdaCase #-}

module TranspilerPython
  ( compileAstToPython,
    compileSymbol,
    compileConstant,
    compileList,
    compileUnary,
    compileBinary,
    compileBlock,
    compileFunctionCall,
    compileFunctionDefinition,
    compileIfElse,
    compileWhile,
    compileFor,
    compileReturn,
    processPythonOutput,
  ) where
import System.IO (withFile, IOMode(..), hPutStr)
import Types (AST(..), Constant(..), BinaryOperator(..), UnaryOperator(..))

processPythonOutput :: Maybe FilePath -> String -> IO ()
processPythonOutput maybeOutputFile pythonCode = case maybeOutputFile of
  Just outputFile -> withFile outputFile WriteMode (`hPutStr` pythonCode)
  Nothing -> putStrLn pythonCode

compileAstToPython :: AST -> String
compileAstToPython ast = case ast of
  Symbol name -> compileSymbol name
  Constant constValue -> compileConstant constValue
  List asts -> compileList asts
  Unary op ast' -> compileUnary op ast'
  Binary op left right -> compileBinary op left right
  Block asts -> compileBlock asts
  FunctionCall name args -> compileFunctionCall name args
  FunctionDefinition name args body -> compileFunctionDefinition name args body
  IfElse condition thenBranch elseBranch -> compileIfElse condition thenBranch elseBranch
  While condition body -> compileWhile condition body
  For initValue cond body -> compileFor initValue cond body
  Return ast' -> compileReturn ast'
  --Print expr -> "print(" ++ compileAstToPython expr ++ ")"
  _ -> error "Unhandled AST type"

compileSymbol :: String -> String
compileSymbol name = name

compileConstant :: Constant -> String
compileConstant = \case
  Int n -> show n
  Float f -> show f
  Bool b -> if b then "True" else "False"
  Char c -> "'" ++ [c] ++ "'"
  String s -> "\"" ++ s ++ "\""
  Null -> "None"

compileList :: [AST] -> String
compileList asts = "[" ++ concatMap (flip (++) ", " . compileAstToPython) asts ++ "]"

compileUnary :: UnaryOperator -> AST -> String
compileUnary op ast =
  let opStr = case op of
        UnaryMinus -> "-"
        Not -> "not "
  in opStr ++ compileAstToPython ast

compileBinary :: BinaryOperator -> AST -> AST -> String
compileBinary op left right =
  let opStr = case op of
        PlusOp -> "+"
        MinusOp -> "-"
        Multiply -> "*"
        Divide -> "/"
        Modulo -> "%"
        And -> "and"
        Or -> "or"
        Equal -> "="
        Equality -> "=="
        NotEqual -> "!="
        LessThan -> "<"
        LessThanOrEqual -> "<="
        GreaterThan -> ">"
        GreaterThanOrEqual -> ">="
  in compileAstToPython left ++ " " ++ opStr ++ " " ++ compileAstToPython right

compileBlock :: [AST] -> String
compileBlock asts = unlines $ map compileAstToPython asts

compileFunctionCall :: String -> [AST] -> String
compileFunctionCall name args = name ++ "(" ++ concatMap (flip (++) ", " . compileAstToPython) args ++ ")"

compileFunctionDefinition :: String -> [String] -> AST -> String
compileFunctionDefinition name args body =
  "def " ++ name ++ "(" ++ concatMap (++ ", ") args ++ "):\n" ++ compileAstToPython body

compileIfElse :: AST -> AST -> AST -> String
compileIfElse condition thenBranch elseBranch =
  "if " ++ compileAstToPython condition ++ ":\n" ++ compileAstToPython thenBranch
  ++ "\nelse:\n" ++ compileAstToPython elseBranch

compileWhile :: AST -> AST -> String
compileWhile condition body =
  "while " ++ compileAstToPython condition ++ ":\n" ++ compileAstToPython body

compileFor :: AST -> AST -> AST -> String
compileFor initValue cond body =
  "for " ++ compileAstToPython initValue ++ " in range(" ++ compileAstToPython cond ++ "):\n" ++ compileAstToPython body

compileReturn :: AST -> String
compileReturn ast = "return " ++ compileAstToPython ast
