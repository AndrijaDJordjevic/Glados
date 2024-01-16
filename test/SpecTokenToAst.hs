{-
-- EPITECH PROJECT, 2023
-- Glados
-- File description:
-- SpecTokenToAst
-}
module SpecTokenToAst (tokenToAstTests) where

import Test.HUnit
import TokenToAst
import Types

symToken :: String -> Token
symToken = Sym

testTransformTokensToAST :: Test
testTransformTokensToAST =
  TestList
    [ "Transforming tokens to AST"
        ~: transformTokensToAST [symToken "x", BinaryOperator Equal, Const (Float 3.14), Special Semicolon]
        @?= Right [Binary Equal (Symbol "x") (Constant (Float 3.14))],
      "Error case: Unprocessed tokens"
        ~: transformTokensToAST [Const (Int 42), Const (Float 3.14), Sym "x"]
        @?= Left "Invalid instruction:\t[Const (Int 42),Const (Float 3.14),Sym \"x\"]",
      "Error case: Parsing failure"
        ~: transformTokensToAST [Sym "x"]
        @?= Left "Invalid assignment:\t[Sym \"x\"]",
      "Error case: Empty input"
        ~: transformTokensToAST []
        @?= Right []
    ]

testParseTokens :: Test
testParseTokens =
  TestList
    [ "Parsing tokens into ASTs"
        ~: parseTokens [symToken "x", BinaryOperator Equal, Const (Float 3.14), Special Semicolon]
        @?= Right ([Binary Equal (Symbol "x") (Constant (Float 3.14))], []),
      "Error case: Parsing failure"
        ~: parseTokens [Sym "x"]
        @?= Left "Invalid assignment:\t[Sym \"x\"]",
      "Parsing empty input"
        ~: parseTokens []
        @?= Right ([], [])
    ]

testParseInstruction :: Test
testParseInstruction =
  TestList
    [ "Parsing variable assignment"
        ~: parseInstruction [symToken "var", symToken "x", BinaryOperator Equal, Const (Float 3.14), Special Semicolon]
        @?= Right (Binary Equal (Symbol "x") (Constant (Float 3.14)), []),
      "Error case: Missing semicolon after assignment"
        ~: parseInstruction [symToken "var", symToken "x", BinaryOperator Equal, Const (Float 3.14)]
        @?= Left "Expected ';' after assignment\t[]",
      "Parsing comment"
        ~: parseInstruction [Special Comment, symToken "var", symToken "x", BinaryOperator Equal, Const (Float 3.14), Special Semicolon]
        @?= Right (Binary Equal (Symbol "x") (Constant (Float 3.14)), []),
      "Parsing if statement"
        ~: parseInstruction [symToken "if", Special OpenParenthesis, symToken "x", Special CloseParenthesis, Special OpenBrace, symToken "return", Const (Int 42), Special Semicolon, Special CloseBrace]
        @?= Right (IfElse (Symbol "x") (Block [Return (Constant (Int 42))]) (Block []), [])
    ]

-- Tests pour parseAssignment
testParseAssignment :: Test
testParseAssignment =
  TestList
    [ "Parsing assignment with function call"
        ~: parseAssignment [Sym "foo", Special OpenParenthesis, Special CloseParenthesis, Special Semicolon]
        @?= (Right (FunctionCall "foo" [], [Special Semicolon])),
      "Parsing assignment with binary expression"
        ~: parseAssignment [Sym "x", BinaryOperator Equal, Const (Int 42), Special Semicolon]
        @?= (Right (Binary Equal (Symbol "x") (Constant (Int 42)), [Special Semicolon]))
        -- Ajoutez d'autres cas de test selon vos besoins
    ]

-- Tests pour parseIfStatement
testParseIfStatement :: Test
testParseIfStatement =
  TestList
    [ "Parsing if statement"
        ~: parseIfStatement [Special OpenParenthesis, Const (Bool True), Special CloseParenthesis, Special OpenBrace, Sym "return", Const (Int 42), Special Semicolon, Special CloseBrace]
        @?= (Right (IfElse (Constant (Bool True)) (Block [Return (Constant (Int 42))]) (Block []), []))
        -- Ajoutez d'autres cas de test selon vos besoins
    ]

-- Tests pour parseWhileStatement
testParseWhileStatement :: Test
testParseWhileStatement =
  TestList
    [ "Parsing while statement"
        ~: parseWhileStatement [Special OpenParenthesis, Const (Bool True), Special CloseParenthesis, Special OpenBrace, Sym "return", Const (Int 42), Special Semicolon, Special CloseBrace]
        @?= (Right (While (Constant (Bool True)) (Block [Return (Constant (Int 42))]), []))
        -- Ajoutez d'autres cas de test selon vos besoins
    ]

-- Tests pour parseFonction
testParseFunction :: Test
testParseFunction =
  TestList
    [ "Parsing function definition without arguments"
        ~: parseFunction [Sym "foo", Special OpenParenthesis, Special CloseParenthesis, Special OpenBrace, Sym "return", Const (Int 42), Special Semicolon, Special CloseBrace]
        @?= (Right (FunctionDefinition "foo" [] (Block [Return (Constant (Int 42))]), []))
        -- Ajoutez d'autres cas de test selon vos besoins
    ]

-- Tests pour parseForStatement
testParseForStatement :: Test
testParseForStatement =
  TestList
    [ "Parsing for statement"
        ~: parseForStatement [Special OpenParenthesis, Const (Int 1), Special Semicolon, Const (Bool True), Special CloseParenthesis, Special OpenBrace, Sym "return", Const (Int 42), Special Semicolon, Special CloseBrace]
        @?= (Right (For (Constant (Int 1)) (Constant (Bool True)) (Block [Return (Constant (Int 42))]), []))
        -- Ajoutez d'autres cas de test selon vos besoins
    ]

-- Tests pour parseReturnStatement
testParseReturnStatement :: Test
testParseReturnStatement =
  TestList
    [ "Parsing return statement"
        ~: assertEqual
          "Result"
          (parseReturnStatement [Const (Int 42), Special Semicolon])
          (Right (Return (Constant (Int 42)), []))
          -- Ajoutez d'autres cas de test selon vos besoins
    ]

-- Tests pour parseBlock
testParseBlock :: Test
testParseBlock =
  TestList
    [ "Parsing block"
        ~: assertEqual
          "Result"
          (parseBlock [Special OpenBrace, Sym "return", Const (Int 42), Special Semicolon, Special CloseBrace])
          (Right (Block [Return (Constant (Int 42))], []))
          -- Ajoutez d'autres cas de test selon vos besoins
    ]

tokenToAstTests :: Test
tokenToAstTests =
  TestList
    [ TestLabel "ParseAssignment" testParseAssignment,
      TestLabel "ParseIfStatement" testParseIfStatement,
      TestLabel "ParseWhileStatement" testParseWhileStatement,
      TestLabel "ParseFonction" testParseFunction,
      TestLabel "ParseForStatement" testParseForStatement,
      TestLabel "ParseReturnStatement" testParseReturnStatement,
      TestLabel "ParseBlock" testParseBlock,
      TestLabel "TransformTokensToAST" testTransformTokensToAST,
      TestLabel "ParseTokens" testParseTokens,
      TestLabel "ParseInstruction" testParseInstruction
    ]
