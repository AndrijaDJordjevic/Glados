-- EPITECH PROJECT, 2024
-- Glados [WSL: Ubuntu]
-- File description:
-- executeInstruction
--

module ExecuteInstructions
  ( executeInstructions,
  )
where

import Builtins.FunctionOperations (assignFunctionParameters, extractFunctionInstructions, loadFunction, markFunctionStart, storeFunction)
import Builtins.StackOperations (addIfVar, executeBinaryOp)
import Builtins.SymValBuiltins (extractValue, getValue, updateVariable)
import Data.List (find)
import Debug.Trace
import Types (BinaryOperator (..), Instruction (..))
import VMTypes (LoopControl (..), NewFunction (..), Stack, StackValue (..))

executeInstruction :: Stack -> Instruction -> Int -> (Stack, Int)
executeInstruction stack instr ip = case instr of
  IPushInt n -> (IntVal n : stack, ip + 1)
  IPushFloat f -> (FloatVal f : stack, ip + 1)
  IPushBool b -> (BoolVal b : stack, ip + 1)
  IPushString s -> (StringVal s : stack, ip + 1)
  -- (c'était un test ce ipush symm ne marche pas) IPushSym varName -> (getValue stack varName : stack, ip + 1)
  IPushNull -> (IntVal 0 : stack, ip + 1)
  IUnaryNot -> case stack of
    BoolVal b : xs -> (BoolVal (not b) : xs, ip + 1)
    _ -> error "Unary NOT not supported for this type"
  IUnaryMinus -> case stack of
    IntVal n : xs -> (IntVal (-n) : xs, ip + 1)
    FloatVal f : xs -> (FloatVal (-f) : xs, ip + 1)
    _ -> error "Unary Minus not supported for this type"
  IPushSym varName ->
    let (_, newStack) = getValue stack varName
     in (newStack, ip + 1)
  IBinaryOp Equal -> case stack of
    newValue : SymVal varName _ : restOfStack ->
      (updateVariable varName newValue restOfStack, ip + 1)
    SymVal varName _ : newValue : restOfStack ->
      (updateVariable varName newValue restOfStack, ip + 1)
    _ -> error "Cannot assign value to non variable"
  IBinaryOp op -> case stack of
    x : y : ys ->
      let xVal = extractValue x
          yVal = extractValue y
          result = executeBinaryOp op yVal xVal
       in ([result] ++ ys ++ addIfVar x (addIfVar y []), ip + 1)
    _ -> error "Not enough values on the stack to execute binary operation"
  IStartLoop ->
    (LoopControl (LoopStart ip) : stack, ip + 1)
  IEndLoop ->
    let loopStart = findLoopStart stack
     in case loopStart of
          Just startIp -> (LoopControl (LoopEnd ip) : stack, startIp)
          Nothing -> error "Loop start not found"
  IPrint -> case stack of
    val : restStack ->
      let output = stackValueToString (extractValue val) -- Convert the value to a string
       in trace output (restStack, ip + 1) -- Print and return the next state
    _ -> error "Stack is empty, cannot print"
  IJumpIfFalse offset -> case stack of
    BoolVal False : rest -> (rest, ip + offset)
    BoolVal True : rest -> (rest, ip + 1)
    _ -> error "Top of stack is not a boolean in IJumpIfFalse"
  IJump offset -> (stack, ip + offset + 1)
  ICallFunc name nbArgs ->
    let func = loadFunction name nbArgs stack
     in case func of
          Just (NewFunc (Start _ args instrs)) ->
            let newStack = assignFunctionParameters args stack
             in (executeInstructions instrs (markFunctionStart newStack name nbArgs) 0, ip + 1)
          Just _ -> error "Function is not a NewFunc"
          Nothing -> error $ "Function " ++ name ++ " not found"
  _ -> error $ "Unhandled instruction: " ++ show instr

executeInstructions :: [Instruction] -> Stack -> Int -> Stack
executeInstructions instrs stack ip
  | ip < 0 || ip >= length instrs = stack
  | otherwise =
      let currentInstr = instrs !! ip
       in case currentInstr of
            IFuncDefStart name args ->
              let (funcInstructions, newIp) = extractFunctionInstructions name [] instrs (ip + 1)
                  newFunc = NewFunc (Start name args funcInstructions)
               in executeInstructions instrs (newFunc : stack) newIp
            IReturn -> case stack of
              [] -> error "Stack is empty"
              returnVal : rest ->
                let newStack = storeFunction rest
                 in executeInstructions instrs (extractValue returnVal : addIfVar returnVal newStack) (ip + 1)
            _ ->
              let (newStack, newIp) = executeInstruction stack currentInstr ip
               in executeInstructions instrs newStack newIp

stackValueToString :: StackValue -> String
stackValueToString val = case val of
  IntVal i -> show i
  FloatVal f -> show f
  BoolVal b -> show b
  StringVal s -> s
  SymVal _ value -> stackValueToString value
  _ -> error "Cannot print this type"

findLoopStart :: Stack -> Maybe Int
findLoopStart stack = case find isLoopStart stack of
  Just (LoopControl (LoopStart ip)) -> Just ip
  _ -> Nothing

isLoopStart :: StackValue -> Bool
isLoopStart (LoopControl (LoopStart _)) = True
isLoopStart _ = False
