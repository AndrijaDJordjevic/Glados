{-
-- EPITECH PROJECT, 2023
-- Glados
-- File description:
-- SpecParser
-}
module SpecParser (parserTests) where

import Data.Char (isDigit, isLetter, toLower)
import Parser (Parser (..), parseAnd, parseChar, parseDouble, parseIntToString, parseNumber, parseString, satisfy)
import Test.HUnit

testParseNumber :: Test
testParseNumber =
  TestList
    [ "Parsing integer"
        ~: runParser parseNumber "123abc5"
        @?= Just (123, "abc5"),
      "Parsing negative integer"
        ~: runParser parseNumber "-12xyz0"
        @?= Just (-12, "xyz0"),
      "Parsing integer with leading zeros"
        ~: runParser parseNumber "00123xyz0"
        @?= Just (123, "xyz0")
    ]

testParseDouble :: Test
testParseDouble =
  TestList
    [ "Parsing double"
        ~: runParser parseDouble "123.456abc0"
        @?= Just (123.456, "abc0"),
      "Parsing negative double"
        ~: runParser parseDouble "-12.34xyz0"
        @?= Just (-12.34, "xyz0"),
      "Parsing double with leading zeros"
        ~: runParser parseDouble "00123.456xyz0"
        @?= Just (123.456, "xyz0")
    ]

testParseString :: Test
testParseString =
  TestList
    [ "Parsing matching string at the beginning"
        ~: runParser (parseString "hello") "hello world"
        @?= Just ("hello", " world"),
      "Parsing non-matching string"
        ~: runParser (parseString "foo") "bar baz"
        @?= Nothing,
      "Parsing empty string"
        ~: runParser (parseString "") "abc"
        @?= Just ("", "abc"),
      "Parsing string with leading whitespaces"
        ~: runParser (parseString "   test") "   test123"
        @?= Just ("   test", "123"),
      "Parsing string with trailing whitespaces"
        ~: runParser (parseString "test   ") "test   123"
        @?= Just ("test   ", "123"),
      "Parsing string with mixed whitespaces"
        ~: runParser (parseString "test string") "test string 123"
        @?= Just ("test string", " 123"),
      "Parsing string with case sensitivity"
        ~: runParser (parseString "CaseSensitive") "casesensitive test"
        @?= Nothing
    ]

testSatisfy :: Test
testSatisfy =
  TestList
    [ "Satisfy with matching character"
        ~: runParser (satisfy (\c -> c == 'a')) "abc"
        @?= Just ('a', "bc"),
      "Satisfy with non-matching character"
        ~: runParser (satisfy (\c -> c == 'a')) "bcd"
        @?= Nothing,
      "Satisfy with empty input"
        ~: runParser (satisfy (\c -> c == 'a')) ""
        @?= Nothing,
      "Satisfy with multiple characters"
        ~: runParser (satisfy (\c -> isDigit c)) "123abc"
        @?= Just ('1', "23abc"),
      "Satisfy with custom predicate"
        ~: runParser (satisfy (\c -> toLower c == 'a')) "ABC"
        @?= Just ('A', "BC"),
      "Satisfy with multiple predicates"
        ~: runParser (satisfy (\c -> isDigit c || isLetter c)) "123xyz"
        @?= Just ('1', "23xyz")
    ]

testParseAnd :: Test
testParseAnd =
  TestList
    [ "Parsing 'hello123' with parseAnd"
        ~: runParser (parseAnd (parseString "hello") parseNumber) "hello123 world"
        @?= Just (("hello", 123), " world"),
      "Parsing 'foo' with parseAnd"
        ~: runParser (parseAnd (parseString "foo") parseNumber) "foo bar"
        @?= Nothing,
      "Parsing '123abc' with parseAnd"
        ~: runParser (parseAnd parseNumber (parseString "abc")) "123abc"
        @?= Just ((123, "abc"), ""),
      "Parsing 'hello' and '123' with parseAnd"
        ~: runParser (parseAnd (parseString "hello") (parseString "123")) "hello123 world"
        @?= Just (("hello", "123"), " world")
    ]

testParseChar :: Test
testParseChar =
  TestList
    [ "Parsing 'abc' with parseChar"
        ~: runParser parseChar "abc"
        @?= Just ('a', "bc"),
      "Parsing '123' with parseChar"
        ~: runParser parseChar "123"
        @?= Just ('1', "23"),
      "Parsing empty input with parseChar"
        ~: runParser parseChar ""
        @?= Nothing,
      "Parsing '   test' with parseChar"
        ~: runParser parseChar "   test"
        @?= Just (' ', "  test")
    ]

testParseIntToString :: Test
testParseIntToString =
  TestList
    [ "Parsing digits to string"
        ~: runParser parseIntToString "123abc"
        @?= Just ("123", "abc"),
      "Parsing empty input"
        ~: runParser parseIntToString ""
        @?= Nothing,
      "Parsing digits with leading zeros"
        ~: runParser parseIntToString "00123xyz"
        @?= Just ("00123", "xyz"),
      "Parsing digits with non-digit characters"
        ~: runParser parseIntToString "12abc"
        @?= Just ("12", "abc"),
      "Parsing digits with trailing non-digit characters"
        ~: runParser parseIntToString "456def"
        @?= Just ("456", "def")
    ]

parserTests :: Test
parserTests =
  TestList
    [ testParseNumber,
      testParseDouble,
      testParseString,
      testSatisfy,
      testParseIntToString,
      testParseAnd,
      testParseChar
    ]
