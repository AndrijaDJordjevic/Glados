{-
-- EPITECH PROJECT, 2023
-- Glados
-- File description:
-- Types
-}
module Types
  ( Constant (..),
    BinaryOperator (..),
    UnaryOperator (..),
    SpecialChar (..),
    Token (..),
    AST (..),
    Instruction (..),
  )
where

data Instruction
  = IPushSym String
  | IPushInt Int
  | IPushFloat Float
  | IPushBool Bool
  | IPushChar Char
  | IPushString String
  | IPushNull
  | IBuildList Int
  | IUnaryNot
  | IUnaryMinus
  | IBinaryOp BinaryOperator
  | IJumpIfFalse Int
  | IJump Int
  | ICallFunc String Int
  | IFuncDefStart String [String]
  | IFuncDefEnd String
  | IStartLoop
  | IEndLoop
  | IReturn
  deriving (Show, Eq)

data Constant
  = Int Integer
  | Float Double
  | Bool Bool
  | Char Char
  | String String
  | Null
  deriving (Show, Eq)

data BinaryOperator
  = PlusOp
  | MinusOp
  | Multiply
  | Divide
  | Modulo
  | And
  | Or
  | Equal
  | Equality
  | NotEqual
  | LessThan
  | LessThanOrEqual
  | GreaterThan
  | GreaterThanOrEqual
  deriving (Show, Eq)

data UnaryOperator
  = UnaryMinus
  | Not
  deriving (Show, Eq)

data SpecialChar
  = Blank
  | Comment
  | OpenParenthesis
  | CloseParenthesis
  | OpenBracket
  | CloseBracket
  | OpenBrace
  | CloseBrace
  | Comma
  | Semicolon
  | Colon
  | Dot
  | QuestionMark
  | ExclamationMark
  | Plus
  | Minus
  | Asterisk
  | Slash
  deriving (Show, Eq)

data Token
  = Const Constant
  | Sym String
  | Special SpecialChar
  | BinaryOperator BinaryOperator
  | UnaryOperator UnaryOperator
  deriving (Show, Eq)

data AST
  = Symbol String
  | Constant Constant
  | List [AST]
  | Unary UnaryOperator AST
  | Binary BinaryOperator AST AST
  | Block [AST]
  | Print AST
  | FunctionCall String [AST]
  | FunctionDefinition String [String] AST
  | IfElse AST AST AST
  | While AST AST
  | For AST AST AST
  | Return AST
  deriving (Show, Eq)
