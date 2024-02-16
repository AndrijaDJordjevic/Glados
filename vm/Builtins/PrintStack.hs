--
-- EPITECH PROJECT, 2024
-- Glados [WSL: Ubuntu]
-- File description:
-- PrintStack
--

module Builtins.PrintStack
  ( printStack,
  )
where

-- import Builtins.SymValBuiltins (intercalateWithBrackets)

import Types ()
import VMTypes
  ( LoadFunction (EndLoad, StartLoad),
    NewFunction (..),
    Stack,
    StackValue (..),
  )

printStack :: Stack -> IO ()
printStack = mapM_ (print . showStackValue)

showStackValue :: StackValue -> String
showStackValue val = case val of
  IntVal i -> show i
  FloatVal f -> show f
  BoolVal b -> show b
  StringVal s -> s
  SymVal name value -> "SymVal: " ++ name ++ " with value: " ++ showStackValue value
  NewFunc (Start name _ _) -> "New function named " ++ name
  LoopControl loopControl -> show loopControl
  LoadFunction (StartLoad name) -> "Load function " ++ show name
  LoadFunction (EndLoad name) -> "End load function " ++ show name

-- showStackValueWithSym :: StackValue -> String
-- showStackValueWithSym (SymVal name value) = "SymVal: " ++ name ++ " with value: " ++ showStackValue value
-- showStackValueWithSym val = showStackValue val
