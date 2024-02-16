{-
-- EPITECH PROJECT, 2023
-- Glados
-- File description:
-- FunctionOperations
-}

module Builtins.FunctionOperations
  ( loadFunction,
    extractFunctionInstructions,
    assignFunctionParameters,
    getReturnVal,
    markFunctionStart,
    storeFunction,
  )
where

import Builtins.SymValBuiltins (updateVariable)
import Data.List (genericTake)
import Types (Instruction (..))
import VMTypes (LoadFunction (..), NewFunction (..), Stack, StackValue (..))

findFunction :: String -> Stack -> Maybe StackValue
findFunction _ [] = Nothing
findFunction funcName (NewFunc (Start name args instrs) : rest) =
  if name == funcName
    then Just (NewFunc (Start name args instrs))
    else findFunction funcName rest
findFunction name (_ : rest) = findFunction name rest

-- getValue :: StackValue -> StackValue
-- getValue (SymVal _ val) = getValue val
-- getValue val = val

addSymValsToStack :: Stack -> [String] -> Stack
addSymValsToStack stack stringList =
  let n = length stringList
      lastNElements = genericTake n stack
      newStack = foldr (\(sym, val) acc -> updateVariable sym val acc) (drop n stack) (zip stringList $ reverse lastNElements)
   in newStack

markFunctionStart :: Stack -> String -> Int -> Stack
markFunctionStart stack name n =
  let (args, rest) = splitAt n stack
   in args ++ [LoadFunction (StartLoad name)] ++ rest

assignFunctionParameters :: [String] -> Stack -> Stack
assignFunctionParameters [] stack = stack
assignFunctionParameters args stack = addSymValsToStack stack args

loadFunction :: String -> Int -> Stack -> Maybe StackValue
loadFunction _ _ [] = Nothing
loadFunction name _ stack = case findFunction name stack of
  Just (NewFunc (Start funcName args instrs)) -> Just (NewFunc (Start funcName args instrs))
  _ -> Nothing

getReturnVal :: Stack -> StackValue
getReturnVal [] = error "Stack is empty"
getReturnVal (LoadFunction StartLoad {} : val : _) = val
getReturnVal (_ : rest) = getReturnVal rest

extractFunctionInstructions :: String -> [Instruction] -> [Instruction] -> Int -> ([Instruction], Int)
extractFunctionInstructions funcName acc instrs ip
  | ip < 0 || ip >= length instrs = error "Function not found"
  | otherwise =
      let currentInstr = instrs !! ip
       in case currentInstr of
            IFuncDefEnd name -> if name == funcName then (acc, ip + 1) else extractFunctionInstructions funcName acc instrs (ip + 1)
            _ -> extractFunctionInstructions funcName (acc ++ [currentInstr]) instrs (ip + 1)

storeFunction :: Stack -> Stack
storeFunction stack = case break isStartLoad stack of
  (_, []) -> stack
  (_, LoadFunction (StartLoad _) : _) -> stack
  _ -> error "This should not happen"


isStartLoad :: StackValue -> Bool
isStartLoad (LoadFunction (StartLoad _)) = True
isStartLoad _ = False
