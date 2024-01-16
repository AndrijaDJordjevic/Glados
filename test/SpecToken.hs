{-
-- EPITECH PROJECT, 2023
-- Glados
-- File description:
-- SpecToken
-}
module SpecToken (tokenTests) where

import Parser (runParser)
import Test.HUnit
import Token (parseBinaryOperator, parseBoolConst, parseCharConst, parseComment, parseConstant, parseFloatConst, parseInput, parseIntConst, parseNullConst, parseSpecialChar, parseStringConst, parseSymbol, parseToken, parseUnaryOperator)
import Types (BinaryOperator (..), Constant (..), SpecialChar (..), Token (..), UnaryOperator (..))

testParseBoolConst :: Test
testParseBoolConst =
  TestList
    [ "Parsing 'True'"
        ~: runParser parseBoolConst "True rest"
        @?= (Just (Const (Bool True), " rest")),
      "Parsing 'False'"
        ~: (runParser parseBoolConst "False rest")
        @?= (Just (Const (Bool False), " rest")),
      "Parsing 'True' with extra characters"
        ~: (runParser parseBoolConst "TrueXYZ rest")
        @?= (Just (Const (Bool True), "XYZ rest")),
      "Parsing 'False' with extra characters"
        ~: (runParser parseBoolConst "FalseXYZ rest")
        @?= (Just (Const (Bool False), "XYZ rest")),
      "Parsing with incorrect input"
        ~: (runParser parseBoolConst "Invalid rest")
        @?= Nothing,
      "Parsing with empty input"
        ~: (runParser parseBoolConst "")
        @?= Nothing,
      "Parsing with unknown input"
        ~: (runParser parseBoolConst "Unknown rest")
        @?= Nothing
    ]

testParseCharConst :: Test
testParseCharConst =
  TestList
    [ "Parsing character 'a'"
        ~: (runParser parseCharConst "'a' rest")
        @?= (Just (Const (Char 'a'), " rest")),
      "Parsing character '1'"
        ~: (runParser parseCharConst "'1' rest")
        @?= (Just (Const (Char '1'), " rest")),
      "Parsing character '!'"
        ~: (runParser parseCharConst "'!' rest")
        @?= (Just (Const (Char '!'), " rest")),
      "Parsing character with extra characters"
        ~: (runParser parseCharConst "'aXYZ' rest")
        @?= Nothing,
      "Parsing with incorrect input (missing closing quote)"
        ~: (runParser parseCharConst "'a rest")
        @?= Nothing,
      "Parsing with incorrect input (multiple characters)"
        ~: (runParser parseCharConst "'abc' rest")
        @?= Nothing,
      "Parsing with empty input"
        ~: (runParser parseCharConst "")
        @?= Nothing
    ]

testParseStringConst :: Test
testParseStringConst =
  TestList
    [ "Parsing string \"hello\""
        ~: (runParser parseStringConst "\"hello\" rest")
        @?= (Just (Const (String "hello"), " rest")),
      "Parsing string with numbers \"abc123\""
        ~: (runParser parseStringConst "\"abc123\" rest")
        @?= (Just (Const (String "abc123"), " rest")),
      "Parsing string with special characters \"!@#$\""
        ~: (runParser parseStringConst "\"!@#$\" rest")
        @?= (Just (Const (String "!@#$"), " rest")),
      "Parsing string with extra characters"
        ~: (runParser parseStringConst "\"helloXYZ\" rest")
        @?= (Just (Const (String "helloXYZ"), " rest")),
      "Parsing with incorrect input (missing closing quote)"
        ~: (runParser parseStringConst "\"hello rest")
        @?= Nothing,
      "Parsing with incorrect input (unclosed escape sequence)"
        ~: (runParser parseStringConst "\"unclosed rest")
        @?= Nothing,
      "Parsing with empty input"
        ~: (runParser parseStringConst "")
        @?= Nothing,
      "Parsing with empty string"
        ~: (runParser parseStringConst "\"\" rest")
        @?= (Just (Const (String ""), " rest"))
    ]

testParseNullConst :: Test
testParseNullConst =
  TestList
    [ "Parsing 'NULL'"
        ~: (runParser parseNullConst "NULL rest")
        @?= (Just (Const Null, " rest")),
      "Parsing 'NULL' with extra characters"
        ~: (runParser parseNullConst "NULLXYZ rest")
        @?= (Just (Const Null, "XYZ rest")),
      "Parsing with incorrect input"
        ~: (runParser parseNullConst "INVALID rest")
        @?= Nothing,
      "Parsing with empty input"
        ~: (runParser parseNullConst "")
        @?= Nothing
    ]

testParseConstant :: Test
testParseConstant =
  TestList
    [ "Parsing float constant"
        ~: runParser parseConstant "3.14xyz"
        @?= Just (Const (Float (3.14)), "xyz"),
      "Parsing integer constant"
        ~: runParser parseConstant "42xyz"
        @?= Just (Const (Int (42)), "xyz")
        -- Add tests for other constant types (Bool, Null, Char, String) as needed
    ]

testParseToken :: Test
testParseToken =
  TestList
    [ "Parsing a constant"
        ~: runParser parseToken "3.14xyz"
        @?= Just (Const (Float (3.14)), "xyz"),
      "Parsing a symbol"
        ~: runParser parseToken "abc123 rest of the input"
        @?= Just (Sym "abc123", " rest of the input")
        -- Add more tests for token parsing
    ]

testParseInput :: Test
testParseInput =
  TestList
    [ "Parsing an input with multiple tokens"
        ~: parseInput "3.14 + abc"
        @?= Left [Const (Float (3.14)), BinaryOperator PlusOp, Sym "abc"],
      "Parsing an input with a comment"
        ~: parseInput "# This is a comment\n123"
        @?= Left [Special Comment, Const (Int (123))]
        -- Add more tests for parseInput
    ]

testParseComment :: Test
testParseComment =
  TestList
    [ "Parsing comment"
        ~: runParser parseComment "# This is a comment\nrest of the input"
        @?= Just (Special Comment, "\nrest of the input"),
      "Parsing comment with no newline"
        ~: runParser parseComment "# Comment without newline"
        @?= Just (Special Comment, "")
    ]

testParseSpecialChar :: Test
testParseSpecialChar =
  TestList
    [ "Parsing '('"
        ~: runParser parseSpecialChar "("
        @?= Just (Special OpenParenthesis, ""),
      "Parsing ')'"
        ~: runParser parseSpecialChar ")rest of the input"
        @?= Just (Special CloseParenthesis, "rest of the input"),
      "Parsing '['"
        ~: runParser parseSpecialChar "[rest of the input"
        @?= Just (Special OpenBracket, "rest of the input"),
      "Parsing ']'"
        ~: runParser parseSpecialChar "]rest of the input"
        @?= Just (Special CloseBracket, "rest of the input"),
      "Parsing '{'"
        ~: runParser parseSpecialChar "{rest of the input"
        @?= Just (Special OpenBrace, "rest of the input"),
      "Parsing '}'"
        ~: runParser parseSpecialChar "}rest of the input"
        @?= Just (Special CloseBrace, "rest of the input"),
      "Parsing ','"
        ~: runParser parseSpecialChar ",rest of the input"
        @?= Just (Special Comma, "rest of the input"),
      "Parsing ';'"
        ~: runParser parseSpecialChar ";rest of the input"
        @?= Just (Special Semicolon, "rest of the input"),
      "Parsing ':'"
        ~: runParser parseSpecialChar ":rest of the input"
        @?= Just (Special Colon, "rest of the input"),
      "Parsing '.'"
        ~: runParser parseSpecialChar ".rest of the input"
        @?= Just (Special Dot, "rest of the input"),
      "Parsing '?'"
        ~: runParser parseSpecialChar "?rest of the input"
        @?= Just (Special QuestionMark, "rest of the input"),
      "Parsing '!'"
        ~: runParser parseSpecialChar "!rest of the input"
        @?= Just (Special ExclamationMark, "rest of the input"),
      "Parsing '+'"
        ~: runParser parseSpecialChar "+rest of the input"
        @?= Just (Special Plus, "rest of the input"),
      "Parsing '-'"
        ~: runParser parseSpecialChar "-rest of the input"
        @?= Just (Special Minus, "rest of the input"),
      "Parsing '*'"
        ~: runParser parseSpecialChar "*rest of the input"
        @?= Just (Special Asterisk, "rest of the input"),
      "Parsing '/'"
        ~: runParser parseSpecialChar "/rest of the input"
        @?= Just (Special Slash, "rest of the input"),
      "Parsing ' '"
        ~: runParser parseSpecialChar " rest of the input"
        @?= Just (Special Blank, "rest of the input"),
      "Parsing '\n'"
        ~: runParser parseSpecialChar "\nrest of the input"
        @?= Just (Special Blank, "rest of the input"),
      "Parsing '\t'"
        ~: runParser parseSpecialChar "\trest of the input"
        @?= Just (Special Blank, "rest of the input"),
      "Parsing comment"
        ~: runParser parseSpecialChar "# Comment\nrest of the input"
        @?= Just (Special Comment, "\nrest of the input")
    ]

testParseSymbol :: Test
testParseSymbol =
  TestList
    [ "Parsing symbol"
        ~: runParser parseSymbol "abc123"
        @?= Just (Sym "abc123", ""),
      "Parsing symbol with underscores"
        ~: runParser parseSymbol "abc_def_123 rest of the input"
        @?= Just (Sym "abc_def_123", " rest of the input")
    ]

testParseBinaryOperator :: Test
testParseBinaryOperator =
  TestList
    [ "Parsing '=='"
        ~: runParser parseBinaryOperator "==xyz"
        @?= Just (BinaryOperator Equality, "xyz"),
      "Parsing '='"
        ~: runParser parseBinaryOperator "=rest"
        @?= Just (BinaryOperator Equal, "rest"),
      "Parsing '!='"
        ~: runParser parseBinaryOperator "!=rest"
        @?= Just (BinaryOperator NotEqual, "rest"),
      "Parsing '<='"
        ~: runParser parseBinaryOperator "<=rest"
        @?= Just (BinaryOperator LessThanOrEqual, "rest"),
      "Parsing '>='"
        ~: runParser parseBinaryOperator ">=rest"
        @?= Just (BinaryOperator GreaterThanOrEqual, "rest"),
      "Parsing '<'"
        ~: runParser parseBinaryOperator "<rest"
        @?= Just (BinaryOperator LessThan, "rest"),
      "Parsing '>"
        ~: runParser parseBinaryOperator ">rest"
        @?= Just (BinaryOperator GreaterThan, "rest"),
      "Parsing '&&'"
        ~: runParser parseBinaryOperator "&&rest"
        @?= Just (BinaryOperator And, "rest"),
      "Parsing '||'"
        ~: runParser parseBinaryOperator "||rest"
        @?= Just (BinaryOperator Or, "rest"),
      "Parsing '+'"
        ~: runParser parseBinaryOperator "+xyz"
        @?= Just (BinaryOperator PlusOp, "xyz"),
      "Parsing '-'"
        ~: runParser parseBinaryOperator "-rest"
        @?= Just (BinaryOperator MinusOp, "rest"),
      "Parsing '*'"
        ~: runParser parseBinaryOperator "*rest"
        @?= Just (BinaryOperator Multiply, "rest"),
      "Parsing '/'"
        ~: runParser parseBinaryOperator "/rest"
        @?= Just (BinaryOperator Divide, "rest"),
      "Parsing '%'"
        ~: runParser parseBinaryOperator "%rest"
        @?= Just (BinaryOperator Modulo, "rest")
    ]

testParseUnaryOperator :: Test
testParseUnaryOperator =
  TestList
    [ "Parsing '!'"
        ~: runParser parseUnaryOperator "!xyz"
        @?= Just (UnaryOperator Not, "xyz"),
      "Parsing '-'"
        ~: runParser parseUnaryOperator "-rest"
        @?= Just (UnaryOperator UnaryMinus, "rest")
        -- Add more tests for unary operator parsing
    ]

testParseIntConst :: Test
testParseIntConst =
  TestList
    [ "Parsing integer constant"
        ~: runParser parseIntConst "123xyz"
        @?= Just (Const (Int 123), "xyz"),
      "Parsing negative integer constant"
        ~: runParser parseIntConst "-42rest"
        @?= Just (Const (Int (-42)), "rest"),
      "Parsing integer constant with leading zeros"
        ~: runParser parseIntConst "00123rest"
        @?= Just (Const (Int 123), "rest")
        -- Add more tests for integer constant parsing
    ]

testParseFloatConst :: Test
testParseFloatConst =
  TestList
    [ "Parsing float constant"
        ~: runParser parseFloatConst "3.14xyz"
        @?= Just (Const (Float 3.14), "xyz"),
      "Parsing negative float constant"
        ~: runParser parseFloatConst "-12.34rest"
        @?= Just (Const (Float (-12.34)), "rest"),
      "Parsing float constant with leading zeros"
        ~: runParser parseFloatConst "00123.456rest"
        @?= Just (Const (Float 123.456), "rest")
        -- Add more tests for float constant parsing
    ]

parserTests :: Test
parserTests =
  TestList
    [ testParseComment,
      testParseSpecialChar,
      testParseSymbol,
      testParseBinaryOperator,
      testParseUnaryOperator,
      testParseConstant,
      testParseIntConst,
      testParseFloatConst,
      testParseToken,
      testParseInput
    ]

tokenTests :: Test
tokenTests =
  TestList
    [ TestLabel "ParseBoolConst" testParseBoolConst,
      TestLabel "ParseCharConst" testParseCharConst,
      TestLabel "ParseStringConst" testParseStringConst,
      TestLabel "ParseNullConst" testParseNullConst,
      TestLabel "ParseConstant" parserTests
    ]
