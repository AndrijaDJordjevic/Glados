module SpecInterpreter (interpreterTests) where

import Interpreter (evaluate)
import Types (AST(..), Constant(..))
import Test.HUnit
import System.IO.Silently (capture_)

-- Test for 'print' function
testPrintFunction :: Test
testPrintFunction = TestCase $ do
    let ast = FunctionCall "print" [Constant (String "Hello")]
    capturedOutput <- capture_ $ evaluate ast
    assertEqual "Should print 'Hello'" "Hello\n" capturedOutput

-- Test for 'len' function
testLenFunction :: Test
testLenFunction = TestCase $ do
    let ast = FunctionCall "len" [Constant (String "Hello")]
    capturedOutput <- capture_ $ evaluate ast
    assertEqual "Length of 'Hello'" "5\n" capturedOutput

-- Test for 'toLower' function
testToLowerFunction :: Test
testToLowerFunction = TestCase $ do
    let ast = FunctionCall "toLower" [Constant (String "HELLO")]
    capturedOutput <- capture_ $ evaluate ast
    assertEqual "Convert to lowercase" "hello\n" capturedOutput

-- Test for 'toUpper' function
testToUpperFunction :: Test
testToUpperFunction = TestCase $ do
    let ast = FunctionCall "toUpper" [Constant (String "hello")]
    capturedOutput <- capture_ $ evaluate ast
    assertEqual "Convert to uppercase" "HELLO\n" capturedOutput

-- Test for 'reverseStr' function
testReverseStrFunction :: Test
testReverseStrFunction = TestCase $ do
    let ast = FunctionCall "reverseStr" [Constant (String "hello")]
    capturedOutput <- capture_ $ evaluate ast
    assertEqual "Reverse string" "olleh\n" capturedOutput

-- Test for 'concatStr' function
testConcatStrFunction :: Test
testConcatStrFunction = TestCase $ do
    let ast = FunctionCall "concatStr" [Constant (String "hello"), Constant (String "world")]
    capturedOutput <- capture_ $ evaluate ast
    assertEqual "Concatenate strings" "helloworld\n" capturedOutput

-- Test for 'strToInt' function
testStrToIntFunction :: Test
testStrToIntFunction = TestCase $ do
    let ast = FunctionCall "strToInt" [Constant (String "123")]
    capturedOutput <- capture_ $ evaluate ast
    assertEqual "Convert string to int" "123\n" capturedOutput

-- Test for 'square' function
testSquareFunction :: Test
testSquareFunction = TestCase $ do
    let ast = FunctionCall "square" [Constant (Int 4)]
    capturedOutput <- capture_ $ evaluate ast
    assertEqual "Square of 4" "16\n" capturedOutput

-- Test for 'isEven' function
testIsEvenFunction :: Test
testIsEvenFunction = TestCase $ do
    let ast = FunctionCall "isEven" [Constant (Int 4)]
    capturedOutput <- capture_ $ evaluate ast
    assertEqual "Check if 4 is even" "#t\n" capturedOutput

-- Test for 'multiply' function
testMultiplyFunction :: Test
testMultiplyFunction = TestCase $ do
    let ast = FunctionCall "multiply" [Constant (Int 2), Constant (Int 3)]
    capturedOutput <- capture_ $ evaluate ast
    assertEqual "Multiply 2 and 3" "6\n" capturedOutput

-- Test for 'subtract' function
testSubtractFunction :: Test
testSubtractFunction = TestCase $ do
    let ast = FunctionCall "subtract" [Constant (Int 5), Constant (Int 3)]
    capturedOutput <- capture_ $ evaluate ast
    assertEqual "Subtract 3 from 5" "2\n" capturedOutput

-- Test for 'isEmptyStr' function
testIsEmptyStrFunction :: Test
testIsEmptyStrFunction = TestCase $ do
    let ast = FunctionCall "isEmptyStr" [Constant (String "")]
    capturedOutput <- capture_ $ evaluate ast
    assertEqual "Check if string is empty" "#t\n" capturedOutput

-- Test for 'countOccur' function
testCountOccurFunction :: Test
testCountOccurFunction = TestCase $ do
    let ast = FunctionCall "countOccur" [Constant (String "hello"), Constant (String "l")]
    capturedOutput <- capture_ $ evaluate ast
    assertEqual "Count occurrences of 'l' in 'hello'" "2\n" capturedOutput

-- Test for 'isOdd' function
testIsOddFunction :: Test
testIsOddFunction = TestCase $ do
    let ast = FunctionCall "isOdd" [Constant (Int 5)]
    capturedOutput <- capture_ $ evaluate ast
    assertEqual "Check if 5 is odd" "#t\n" capturedOutput

-- Test for 'intToStr' function
testIntToStrFunction :: Test
testIntToStrFunction = TestCase $ do
    let ast = FunctionCall "intToStr" [Constant (Int 123)]
    capturedOutput <- capture_ $ evaluate ast
    assertEqual "Convert int to string" "123\n" capturedOutput

-- Test suite
interpreterTests :: Test
interpreterTests =
    TestList
    [ 
        TestLabel "PrintFunction" testPrintFunction,
        TestLabel "LenFunction" testLenFunction,
        TestLabel "ToLowerFunction" testToLowerFunction,
        TestLabel "ToUpperFunction" testToUpperFunction,
        TestLabel "ReverseStrFunction" testReverseStrFunction,
        TestLabel "ConcatStrFunction" testConcatStrFunction,
        TestLabel "StrToIntFunction" testStrToIntFunction,
        TestLabel "SquareFunction" testSquareFunction,
        TestLabel "IsEvenFunction" testIsEvenFunction,
        TestLabel "MultiplyFunction" testMultiplyFunction,
        TestLabel "SubtractFunction" testSubtractFunction,
        TestLabel "IsEmptyStrFunction" testIsEmptyStrFunction,
        TestLabel "CountOccurFunction" testCountOccurFunction,
        TestLabel "IsOddFunction" testIsOddFunction,
        TestLabel "IntToStrFunction" testIntToStrFunction, 
        TestLabel "PrintFunction" testPrintFunction,
        TestLabel "LenFunction" testLenFunction,
        TestLabel "ToLowerFunction" testToLowerFunction,
        TestLabel "ToUpperFunction" testToUpperFunction,
        TestLabel "ReverseStrFunction" testReverseStrFunction,
        TestLabel "ConcatStrFunction" testConcatStrFunction,
        TestLabel "StrToIntFunction" testStrToIntFunction,
        TestLabel "SquareFunction" testSquareFunction,
        TestLabel "IsEvenFunction" testIsEvenFunction,
        TestLabel "MultiplyFunction" testMultiplyFunction,
        TestLabel "SubtractFunction" testSubtractFunction,
        TestLabel "IsEmptyStrFunction" testIsEmptyStrFunction,
        TestLabel "CountOccurFunction" testCountOccurFunction,
        TestLabel "IsOddFunction" testIsOddFunction,
        TestLabel "IntToStrFunction" testIntToStrFunction
    ]
