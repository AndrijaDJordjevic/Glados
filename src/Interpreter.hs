module Interpreter (evaluate) where

import Types
import Data.Char (toLower, toUpper)
import Text.Read (readMaybe)
import qualified Data.Text as T

-- Evaluate a single AST node
evaluate :: AST -> IO ()
evaluate (FunctionCall "print" args) = mapM_ printArg args
evaluate (FunctionCall "len" args) = mapM_ lenArg args
evaluate (FunctionCall "toLower" args) = mapM_ toLowerArg args
evaluate (FunctionCall "toUpper" args) = mapM_ toUpperArg args
evaluate (FunctionCall "reverseStr" args) = mapM_ reverseStrArg args
evaluate (FunctionCall "concatStr" [a, b]) = concatStrArg a b
evaluate (FunctionCall "strToInt" [a]) = strToIntArg a
evaluate (FunctionCall "square" [a]) = squareArg a
evaluate (FunctionCall "isEven" [a]) = isEvenArg a
evaluate (FunctionCall "multiply" [a, b]) = multiplyArg a b
evaluate (FunctionCall "subtract" [a, b]) = subtractArg a b
evaluate (FunctionCall "isEmptyStr" [a]) = isEmptyStrArg a
evaluate (FunctionCall "countOccur" [a, b]) = countOccurArg a b
evaluate (FunctionCall "isOdd" [a]) = isOddArg a
evaluate (FunctionCall "intToStr" [a]) = intToStrArg a
evaluate _ = return ()

printArg :: AST -> IO ()
printArg (Constant (String s)) = putStrLn s
printArg _ = putStrLn "Error: Invalid argument to print"

lenArg :: AST -> IO ()
lenArg (Constant (String s)) = print $ length s
lenArg _ = putStrLn "Error: Invalid argument to len"

toLowerArg :: AST -> IO ()
toLowerArg (Constant (String s)) = putStrLn $ map toLower s
toLowerArg _ = putStrLn "Error: Invalid argument to toLower"

toUpperArg :: AST -> IO ()
toUpperArg (Constant (String s)) = putStrLn $ map toUpper s
toUpperArg _ = putStrLn "Error: Invalid argument to toUpper"

reverseStrArg :: AST -> IO ()
reverseStrArg (Constant (String s)) = putStrLn $ reverse s
reverseStrArg _ = putStrLn "Error: Invalid argument to reverseStr"

concatStrArg :: AST -> AST -> IO ()
concatStrArg (Constant (String s1)) (Constant (String s2)) = putStrLn (s1 ++ s2)
concatStrArg _ _ = putStrLn "Error: Invalid arguments to concatStr"

strToIntArg :: AST -> IO ()
strToIntArg (Constant (String s)) = case readMaybe s of
    Just n -> print (n :: Int)
    Nothing -> putStrLn "Error: Cannot convert string to int"
strToIntArg _ = putStrLn "Error: Invalid argument to strToInt"

squareArg :: AST -> IO ()
squareArg (Constant (Int n)) = print (n * n)
squareArg _ = putStrLn "Error: Invalid argument to square"

isEvenArg :: AST -> IO ()
isEvenArg (Constant (Int n)) = putStrLn $ if even n then "#t" else "#f"
isEvenArg _ = putStrLn "Error: Invalid argument to isEven"

multiplyArg :: AST -> AST -> IO ()
multiplyArg (Constant (Int a)) (Constant (Int b)) = print (a * b)
multiplyArg _ _ = putStrLn "Error: Invalid arguments to multiply"

subtractArg :: AST -> AST -> IO ()
subtractArg (Constant (Int a)) (Constant (Int b)) = print (a - b)
subtractArg _ _ = putStrLn "Error: Invalid arguments to subtract"

isEmptyStrArg :: AST -> IO ()
isEmptyStrArg (Constant (String s)) = putStrLn $ if null s then "#t" else "#f"
isEmptyStrArg _ = putStrLn "Error: Invalid argument to isEmptyStr"

countOccurArg :: AST -> AST -> IO ()
countOccurArg (Constant (String str)) (Constant (String subStr)) =
    print $ T.count (T.pack subStr) (T.pack str)
countOccurArg _ _ = putStrLn "Error: Invalid arguments to countOccur"

isOddArg :: AST -> IO ()
isOddArg (Constant (Int a)) = putStrLn $ if a `mod` 2 /= 0 then "#t" else "#f"
isOddArg _ = putStrLn "Error: Invalid argument to isOdd"

intToStrArg :: AST -> IO ()
intToStrArg (Constant (Int a)) = putStrLn $ show a
intToStrArg _ = putStrLn "Error: Invalid argument to intToStr"
