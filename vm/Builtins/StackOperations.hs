--
-- EPITECH PROJECT, 2024
-- Glados [WSL: Ubuntu]
-- File description:
-- Opertator
--

module Builtins.StackOperations (
    addStack,
    subStack,
    mulStack,
    divStack,
    modStack,
    andStack,
    orStack,
    equal,
    equality,
    notEqual,
    lessThan,
    lessThanOrEqual,
    greaterThan,
    greaterThanOrEqual,
    executeBinaryOp,
    addIfVar,
) where

import VMTypes (StackValue(..), Stack())
import Types (BinaryOperator(..))

addIfVar :: StackValue -> Stack -> Stack
addIfVar (SymVal name val) stack = SymVal name val : stack
addIfVar _ stack = stack

executeBinaryOp :: BinaryOperator -> StackValue -> StackValue -> StackValue
executeBinaryOp PlusOp = addStack
executeBinaryOp MinusOp = subStack
executeBinaryOp Multiply = mulStack
executeBinaryOp Divide = divStack
executeBinaryOp Modulo = modStack
executeBinaryOp And = andStack
executeBinaryOp Or = orStack
executeBinaryOp Equality =  equality
executeBinaryOp NotEqual = notEqual
executeBinaryOp LessThan = lessThan
executeBinaryOp LessThanOrEqual = lessThanOrEqual
executeBinaryOp GreaterThan = greaterThan
executeBinaryOp GreaterThanOrEqual = greaterThanOrEqual
executeBinaryOp op = \_ _ -> error $ "this binary operator: '" ++ show op ++ "' is not implemented"

boolToInt :: Bool -> Int
boolToInt True = 1
boolToInt False = 0

addStack :: StackValue -> StackValue -> StackValue
addStack (IntVal a) (IntVal b) = IntVal (a + b)
addStack (FloatVal a) (FloatVal b) = FloatVal (a + b)
addStack (IntVal a) (FloatVal b) = FloatVal (fromIntegral a + b)
addStack (FloatVal a) (IntVal b) = FloatVal (a + fromIntegral b)
addStack (BoolVal a) (BoolVal b) = IntVal (boolToInt a + boolToInt b)
addStack (IntVal a) (BoolVal b) = IntVal (a + boolToInt b)
addStack (BoolVal a) (IntVal b) = IntVal (boolToInt a + b)
addStack (FloatVal a) (BoolVal b) = FloatVal (a + fromIntegral (boolToInt b))
addStack (BoolVal a) (FloatVal b) = FloatVal (fromIntegral (boolToInt a) + b)
addStack (StringVal a) (StringVal b) = StringVal (a ++ b)
addStack (StringVal a) (IntVal b) = StringVal (a ++ show b)
addStack (IntVal a) (StringVal b) = StringVal (show a ++ b)
addStack (StringVal a) (FloatVal b) = StringVal (a ++ show b)
addStack (FloatVal a) (StringVal b) = StringVal (show a ++ b)
addStack (StringVal a) (BoolVal b) = StringVal (a ++ show (boolToInt b))
addStack (BoolVal a) (StringVal b) = StringVal (show (boolToInt a) ++ b)
addStack _ _ = error "Addition not supported for this types"

subStack :: StackValue -> StackValue -> StackValue
subStack (IntVal a) (IntVal b) = IntVal (a - b)
subStack (FloatVal a) (FloatVal b) = FloatVal (a - b)
subStack (IntVal a) (FloatVal b) = FloatVal (fromIntegral a - b)
subStack (FloatVal a) (IntVal b) = FloatVal (a - fromIntegral b)
subStack (BoolVal a) (BoolVal b) = IntVal (boolToInt a - boolToInt b)
subStack (IntVal a) (BoolVal b) = IntVal (a - boolToInt b)
subStack (BoolVal a) (IntVal b) = IntVal (boolToInt a - b)
subStack (FloatVal a) (BoolVal b) = FloatVal (a - fromIntegral (boolToInt b))
subStack (BoolVal a) (FloatVal b) = FloatVal (fromIntegral (boolToInt a) - b)
subStack _ _ = error "substraction not supported for this types"

mulStack :: StackValue -> StackValue -> StackValue
mulStack (IntVal a) (IntVal b) = IntVal (a * b)
mulStack (FloatVal a) (FloatVal b) = FloatVal (a * b)
mulStack (IntVal a) (FloatVal b) = FloatVal (fromIntegral a * b)
mulStack (FloatVal a) (IntVal b) = FloatVal (a * fromIntegral b)
mulStack (BoolVal a) (BoolVal b) = IntVal (boolToInt a * boolToInt b)
mulStack (IntVal a) (BoolVal b) = IntVal (a * boolToInt b)
mulStack (BoolVal a) (IntVal b) = IntVal (boolToInt a * b)
mulStack (FloatVal a) (BoolVal b) = FloatVal (a * fromIntegral (boolToInt b))
mulStack (BoolVal a) (FloatVal b) = FloatVal (fromIntegral (boolToInt a) * b)
mulStack _ _ = error "Multiplication non supportée pour ces types"

divStack :: StackValue -> StackValue -> StackValue
divStack (IntVal a) (IntVal b) = intDiv a b
divStack (FloatVal a) (FloatVal b) = floatDiv a b
divStack (IntVal a) (FloatVal b) = floatDiv (fromIntegral a) b
divStack (FloatVal a) (IntVal b) = floatDiv a (fromIntegral b)
divStack (BoolVal a) (BoolVal b) = intDiv (boolToInt a) (boolToInt b)
divStack (IntVal a) (BoolVal b) = intDiv a (boolToInt b)
divStack (BoolVal a) (IntVal b) = intDiv (boolToInt a) b
divStack (FloatVal a) (BoolVal b) = floatDiv a (fromIntegral (boolToInt b))
divStack (BoolVal a) (FloatVal b) = floatDiv (fromIntegral (boolToInt a)) b
divStack _ _ = error "division not supported for this types"

intDiv :: Int -> Int -> StackValue
intDiv _ 0 = error "Division par zéro"
intDiv a b = IntVal (a `div` b)

floatDiv :: Float -> Float -> StackValue
floatDiv _ 0.0 = error "Division par zéro"
floatDiv a b = FloatVal (a / b)

modStack :: StackValue -> StackValue -> StackValue
modStack (IntVal a) (IntVal b) = intMod a b
modStack (BoolVal a) (BoolVal b) = intMod (boolToInt a) (boolToInt b)
modStack (IntVal a) (BoolVal b) = intMod a (boolToInt b)
modStack (BoolVal a) (IntVal b) = intMod (boolToInt a) b
modStack _ _ = error "modulo not supported for this types"

intMod :: Int -> Int -> StackValue
intMod _ 0 = error "Modulo par zéro"
intMod a b = IntVal (a `mod` b)

andStack :: StackValue -> StackValue -> StackValue
andStack (BoolVal a) (BoolVal b) = BoolVal (a && b)
andStack (IntVal a) (IntVal b) = BoolVal (a /= 0 && b /= 0)
andStack (FloatVal a) (FloatVal b) = BoolVal (a /= 0.0 && b /= 0.0)
andStack (IntVal a) (BoolVal b) = BoolVal (a /= 0 && b)
andStack (BoolVal a) (IntVal b) = BoolVal (a && b /= 0)
andStack (FloatVal a) (BoolVal b) = BoolVal (a /= 0.0 && b)
andStack (BoolVal a) (FloatVal b) = BoolVal (a && b /= 0.0)
andStack _ _ = error "Opération AND non supportée pour ces types"

orStack :: StackValue -> StackValue -> StackValue
orStack (BoolVal a) (BoolVal b) = BoolVal (a || b)
orStack (IntVal a) (IntVal b) = BoolVal (a /= 0 || b /= 0)
orStack (FloatVal a) (FloatVal b) = BoolVal (a /= 0.0 || b /= 0.0)
orStack (IntVal a) (BoolVal b) = BoolVal (a /= 0 || b)
orStack (BoolVal a) (IntVal b) = BoolVal (a || b /= 0)
orStack (FloatVal a) (BoolVal b) = BoolVal (a /= 0.0 || b)
orStack (BoolVal a) (FloatVal b) = BoolVal (a || b /= 0.0)
orStack _ _ = error "or not supported for this types"

equal :: StackValue -> Stack -> Stack
equal symVal@(SymVal _ _) stack = symVal : stack
equal _ _ = error "Equal non supporté pour ces types"

equality :: StackValue -> StackValue -> StackValue
equality (IntVal a) (IntVal b) = BoolVal (a == b)
equality (FloatVal a) (IntVal b) = BoolVal (a == fromIntegral b)
equality (IntVal a) (FloatVal b) = BoolVal (fromIntegral a == b)
equality (FloatVal a) (FloatVal b) = BoolVal (a == b)
equality (BoolVal a) (BoolVal b) = BoolVal (a == b)
equality (StringVal a) (StringVal b) = BoolVal (a == b)
equality _ _ = BoolVal False

notEqual :: StackValue -> StackValue -> StackValue
notEqual a b = BoolVal (not $ val $ equality a b)
    where val (BoolVal boolValue) = boolValue
          val _ = error "not equal not supported for this types"

lessThan :: StackValue -> StackValue -> StackValue
lessThan (IntVal a) (IntVal b) = BoolVal (a < b)
lessThan (FloatVal a) (FloatVal b) = BoolVal (a < b)
lessThan (IntVal a) (FloatVal b) = BoolVal (fromIntegral a < b)
lessThan (FloatVal a) (IntVal b) = BoolVal (a < fromIntegral b)
lessThan (BoolVal a) (BoolVal b) = BoolVal (boolToInt a < boolToInt b)
lessThan _ _ = error "less not supported for this types"

lessThanOrEqual :: StackValue -> StackValue -> StackValue
lessThanOrEqual (IntVal a) (IntVal b) = BoolVal (a <= b)
lessThanOrEqual (FloatVal a) (FloatVal b) = BoolVal (a <= b)
lessThanOrEqual (IntVal a) (FloatVal b) = BoolVal (fromIntegral a <= b)
lessThanOrEqual (FloatVal a) (IntVal b) = BoolVal (a <= fromIntegral b)
lessThanOrEqual (BoolVal a) (BoolVal b) = BoolVal (boolToInt a <= boolToInt b)
lessThanOrEqual _ _ = error "lessorequal not supported for this types"

greaterThan :: StackValue -> StackValue -> StackValue
greaterThan (IntVal a) (IntVal b) = BoolVal (a > b)
greaterThan (FloatVal a) (FloatVal b) = BoolVal (a > b)
greaterThan (IntVal a) (FloatVal b) = BoolVal (fromIntegral a > b)
greaterThan (FloatVal a) (IntVal b) = BoolVal (a > fromIntegral b)
greaterThan (BoolVal a) (BoolVal b) = BoolVal (boolToInt a > boolToInt b)
greaterThan _ _ = error "greater not supported for this types"

greaterThanOrEqual :: StackValue -> StackValue -> StackValue
greaterThanOrEqual (IntVal a) (IntVal b) = BoolVal (a >= b)
greaterThanOrEqual (FloatVal a) (FloatVal b) = BoolVal (a >= b)
greaterThanOrEqual (IntVal a) (FloatVal b) = BoolVal (fromIntegral a >= b)
greaterThanOrEqual (FloatVal a) (IntVal b) = BoolVal (a >= fromIntegral b)
greaterThanOrEqual (BoolVal a) (BoolVal b) = BoolVal (boolToInt a >= boolToInt b)
greaterThanOrEqual _ _ = error "modulo not supported for this types"
