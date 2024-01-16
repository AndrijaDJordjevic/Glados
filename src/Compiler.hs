--
-- EPITECH PROJECT, 2024
-- Glados [WSL: Ubuntu]
-- File description:
-- Compiler
--

module Compiler
  ( compile,
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
    printCompilationResult,
    concatenateInstructions,
  )
where

import Types (AST (..), BinaryOperator (..), Constant (..), Instruction (..), UnaryOperator (..))

concatenateInstructions :: [AST] -> [Instruction]
concatenateInstructions = concatMap compile

compile :: AST -> [Instruction]
compile (Symbol s) = [IPushSym s]
compile (Constant c) = compileConstant c
compile (List lst) = compileList lst
compile (Unary op ast) = compileUnary op ast
compile (Binary op left right) = compileBinary op left right
compile (Block stmts) = compileBlock stmts
compile (FunctionCall name args) = compileFunctionCall name args
compile (FunctionDefinition name args body) = compileFunctionDefinition name args body
compile (IfElse cond trueBranch falseBranch) = compileIfElse cond trueBranch falseBranch
compile (While cond body) = compileWhile cond body
compile (For first cond second) = compileFor first cond second
compile (Return expr) = compileReturn expr

compileConstant :: Constant -> [Instruction]
compileConstant (Int i) = [IPushInt (fromIntegral i)]
compileConstant (Float f) = [IPushFloat (realToFrac f)]
compileConstant (Bool b) = [IPushBool b]
compileConstant (Char c) = [IPushChar c]
compileConstant (String s) = [IPushString s]
compileConstant Null = [IPushNull]

compileList :: [AST] -> [Instruction]
compileList lst = concatMap compile lst ++ [IBuildList $ length lst]

compileUnary :: UnaryOperator -> AST -> [Instruction]
compileUnary Not ast = compile ast ++ [IUnaryNot]
compileUnary UnaryMinus ast = compile ast ++ [IUnaryMinus]

compileBinary :: BinaryOperator -> AST -> AST -> [Instruction]
compileBinary op left right = compile left ++ compile right ++ [IBinaryOp op]

compileBlock :: [AST] -> [Instruction]
compileBlock stmts = concatMap compile stmts

compileFunctionCall :: String -> [AST] -> [Instruction]
compileFunctionCall name args = concatMap compile args ++ [ICallFunc name $ length args]

compileFunctionDefinition :: String -> [String] -> AST -> [Instruction]
compileFunctionDefinition name args body = [IFuncDefStart name args] ++ compile body ++ [IFuncDefEnd name]

compileIfElse :: AST -> AST -> AST -> [Instruction]
compileIfElse cond trueBranch falseBranch =
  compile cond
    ++ [IJumpIfFalse $ length trueBranchCode + 1]
    ++ trueBranchCode
    ++ [IJump $ length falseBranchCode]
    ++ falseBranchCode
  where
    trueBranchCode = compile trueBranch
    falseBranchCode = compile falseBranch

compileWhile :: AST -> AST -> [Instruction]
compileWhile cond body =
  [IStartLoop]
    ++ compile cond
    ++ [IJumpIfFalse $ length bodyCode + 2]
    ++ bodyCode
    ++ [IJump $ -(length condCode + length bodyCode + 2)]
    ++ [IEndLoop]
  where
    condCode = compile cond
    bodyCode = compile body

compileFor :: AST -> AST -> AST -> [Instruction]
compileFor first cond second = compile first ++ compileWhile cond (Block [second])

compileReturn :: AST -> [Instruction]
compileReturn expr = compile expr ++ [IReturn]

showInstruction :: Instruction -> String
showInstruction (IPushSym s) = "PUSH_SYM " ++ s
showInstruction (IPushInt i) = "PUSH_INT " ++ show i
showInstruction (IPushFloat f) = "PUSH_FLOAT " ++ show f
showInstruction (IPushBool b) = "PUSH_BOOL " ++ show b
showInstruction (IPushChar c) = "PUSH_CHAR " ++ show c
showInstruction (IPushString s) = "PUSH_STRING " ++ s
showInstruction IPushNull = "PUSH_NULL"
showInstruction (IBuildList n) = "BUILD_LIST " ++ show n
showInstruction IUnaryNot = "UNRAY_NOT"
showInstruction IUnaryMinus = "UNARY_MINUS"
showInstruction (IBinaryOp op) = "BINARY_OP " ++ show op
showInstruction (IJumpIfFalse n) = "JUMP_IF_FALSE " ++ show n
showInstruction (IJump n) = "JUMP " ++ show n
showInstruction (ICallFunc name n) = "CALLFUNC " ++ name ++ " " ++ show n
showInstruction (IFuncDefStart name args) = "FUNC_DEF_START " ++ name ++ " " ++ show args
showInstruction (IFuncDefEnd name) = "FUNC_DEF_END " ++ name
showInstruction IStartLoop = "STARTLOOP"
showInstruction IEndLoop = "ENDLOOP"
showInstruction IReturn = "RETURN"

printCompilationResult :: [Instruction] -> IO ()
printCompilationResult instrs = mapM_ (putStrLn . showInstruction) instrs
