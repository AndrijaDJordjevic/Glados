{-
-- EPITECH PROJECT, 2023
-- Glados
-- File description:
-- SpecArguments
-}
module SpecArguments (argumentsTests) where

import Arguments (Conf (..), Mode (..), checkArgs, defaultConf)
import Test.HUnit

testCheckArgs :: Test
testCheckArgs =
  TestList
    [ "Valid input with Binary mode"
        ~: checkArgs defaultConf {help = False, mode = Just Binary, input = Just "file.txt", output = Just "output.txt"}
        @?= Just (defaultConf {help = False, mode = Just Binary, input = Just "file.txt", output = Just "output.txt"}),
      "Valid input with Text mode"
        ~: checkArgs defaultConf {help = False, mode = Just Text, input = Just "file.txt", output = Just "output.txt"}
        @?= Just (defaultConf {help = False, mode = Just Text, input = Just "file.txt", output = Just "output.txt"}),
      "Valid input with Python mode"
        ~: checkArgs defaultConf {help = False, mode = Just Python, input = Just "file.txt", output = Just "output.txt"}
        @?= Just (defaultConf {help = False, mode = Just Python, input = Just "file.txt", output = Just "output.txt"}),
      "No mode specified"
        ~: checkArgs defaultConf {help = False, input = Just "file.txt", output = Just "output.txt"}
        @?= Nothing,
      "Empty input"
        ~: checkArgs defaultConf {help = False, mode = Just Binary, input = Just [], output = Just "output.txt"}
        @?= Nothing,
      "Invalid output"
        ~: checkArgs defaultConf {help = False, mode = Just Binary, input = Just "file.txt", output = Nothing}
        @?= Nothing,
      "Help mode"
        ~: checkArgs defaultConf {help = True, mode = Just Binary, input = Just "file.txt", output = Just "output.txt"}
        @?= Just
          (defaultConf {help = True, mode = Just Binary, input = Just "file.txt", output = Just "output.txt"}),
      "Valid lib"
        ~: checkArgs defaultConf {help = False, mode = Just Binary, input = Just "file.txt", output = Just "output.txt", lib = Just "./lib/"}
        @?= Just (defaultConf {help = False, mode = Just Binary, input = Just "file.txt", output = Just "output.txt", lib = Just "./lib/"}),
      "No lib"
        ~: checkArgs defaultConf {help = False, mode = Just Binary, input = Just "file.txt", output = Just "output.txt", lib = Nothing}
        @?= Just (defaultConf {help = False, mode = Just Binary, input = Just "file.txt", output = Just "output.txt", lib = Nothing})
    ]

-- Test suite
argumentsTests :: Test
argumentsTests =
  TestList
    [ TestLabel "Check arguments" testCheckArgs
    ]
