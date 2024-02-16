--
-- EPITECH PROJECT, 2024
-- Glados [WSL: Ubuntu]
-- File description:
-- symValBuiltins
--

module Builtins.SymValBuiltins (
    getValue,
    extractVar,
    extractValue,
    isSameVar,
    updateVariable,
) where

import VMTypes(Stack, StackValue(..))

updateVariable :: String -> StackValue -> Stack -> Stack
updateVariable varName newValue stack = case newValue of
    SymVal _ _ -> updateVariable varName (extractValue newValue) stack
    _ -> let newVar = SymVal varName newValue
             updatedStack = newVar : filter (not . isSameVar varName) stack
         in updatedStack

getValue :: Stack -> String -> (StackValue, Stack)
getValue [] varName = (SymVal varName (IntVal 0), [SymVal varName (IntVal 0)])
getValue stack varName =
    let (maybeVar, rest) = extractVar stack varName
    in case maybeVar of
         Just var -> (var, var : stack)--updateVariable varName var rest)
         Nothing -> let newVar = SymVal varName (IntVal 0) in (newVar, newVar : rest)

extractVar :: Stack -> String -> (Maybe StackValue, Stack)
extractVar [] _ = (Nothing, [])
extractVar (val@(SymVal name _):xs) varName
    | name == varName = (Just val, xs)
    | otherwise = let (foundVar, newStack) = extractVar xs varName in (foundVar, val : newStack)
extractVar (x:xs) varName = let (foundVar, newStack) = extractVar xs varName in (foundVar, x : newStack)

extractValue :: StackValue -> StackValue
extractValue (SymVal _ val) = extractValue val
extractValue val = val

isSameVar :: String -> StackValue -> Bool
isSameVar name (SymVal n _) = name == n
isSameVar _ _ = False
