--
-- EPITECH PROJECT, 2024
-- Glados [WSL: Ubuntu]
-- File description:
-- ParseInstructions
--
module ParseInstructions (
    parseInstructions,
    readAndParseInstructionFile
) where

import Data.Maybe (catMaybes)
import FileReader (readFileSafe)
import Types (Instruction(..), BinaryOperator(..))

parseBinaryOperator :: String -> Maybe BinaryOperator
parseBinaryOperator op = case op of
    "PlusOp" -> Just PlusOp
    "MinusOp" -> Just MinusOp
    "Multiply" -> Just Multiply
    "Divide" -> Just Divide
    "Modulo" -> Just Modulo
    "And" -> Just And
    "Or" -> Just Or
    "Equal" -> Just Equal
    "Equality" -> Just Equality
    "NotEqual" -> Just NotEqual
    "LessThan" -> Just LessThan
    "LessThanOrEqual" -> Just LessThanOrEqual
    "GreaterThan" -> Just GreaterThan
    "GreaterThanOrEqual" -> Just GreaterThanOrEqual
    _ -> Nothing

parseStringToList :: String -> [String]
parseStringToList str
  | str == "[]" = []
  | head str == '[' && last str == ']' = readList' (tail (init str)) ""
  | otherwise = error "Invalid list"

readList' :: String -> String -> [String]
readList' [] acc = [trimQuotes acc]
readList' (',' : rest) acc = trimQuotes acc : readList' rest ""
readList' (c : rest) acc = readList' rest (acc ++ [c])

trimQuotes :: String -> String
trimQuotes = filter (/= '"')

parseInstruction :: String -> Maybe Instruction
parseInstruction str =
    case words str of
        ("IPushSym":s:_) -> Just $ IPushSym (read s)
        ("IPushInt":i:_) -> Just $ IPushInt (read i)
        ("IPushFloat":f:_) -> Just $ IPushFloat (read f)
        ("IPushBool":b:_) -> Just $ IPushBool (read b)
        ("IPushChar":c:_) -> Just $ IPushChar (read c)
        ("IPushString":s:_) -> Just $ IPushString (read s)
        ("IPushNull":_) -> Just IPushNull
        ("IBuildList":n:_) -> Just $ IBuildList (read n)
        ("IUnaryNot":_) -> Just IUnaryNot
        ("IUnaryMinus":_) -> Just IUnaryMinus
        ("IBinaryOp":op:_) -> case parseBinaryOperator op of
            Just binaryOp -> Just $ IBinaryOp binaryOp
            Nothing -> Nothing
        ("IJumpIfFalse":n:_) -> Just $ IJumpIfFalse (read n)
        ("IJump":n:_) -> Just $ IJump (read n)
        ("ICallFunc":name:n:_) -> Just $ ICallFunc name (read n)
        ("IFuncDefStart":name:argsStr) -> Just $ IFuncDefStart name (parseStringToList $ unwords argsStr)
        ("IFuncDefEnd":name:_) -> Just $ IFuncDefEnd name
        ("IStartLoop":_) -> Just IStartLoop
        ("IEndLoop":_) -> Just IEndLoop
        ("IPrint":_) -> Just IPrint
        ("IReturn":_) -> Just IReturn
        _ -> Nothing

parseInstructions :: String -> [Instruction]
parseInstructions = catMaybes . map parseInstruction . lines

readAndParseInstructionFile :: FilePath -> IO [Instruction]
readAndParseInstructionFile path = do
    fileContent <- readFileSafe path
    return $ parseInstructions fileContent