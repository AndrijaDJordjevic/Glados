--
-- EPITECH PROJECT, 2024
-- Glados [WSL: Ubuntu]
-- File description:
-- VMTypes
--

module VMTypes
  ( StackValue (..),
    Stack,
    LoadFunction (..),
    NewFunction (..),
    LoopControl (..),
  )
where

import Types (Instruction)

data LoadFunction
  = StartLoad String
  | EndLoad String
  deriving (Show)

data NewFunction
  = Start String [String] [Instruction]
  deriving (Show)

data StackValue
  = IntVal Int
  | FloatVal Float
  | BoolVal Bool
  | StringVal String
  | SymVal String StackValue
  | LoadFunction LoadFunction
  | NewFunc NewFunction
  | LoopControl LoopControl
  deriving (Show)

data LoopControl = LoopStart Int | LoopEnd Int deriving (Show)

type Stack = [StackValue]
