{-
-- EPITECH PROJECT, 2023
-- Glados
-- File description:
-- Arguments
-}
module Arguments (Conf (..), Mode (..), defaultConf, parseArgs, displayHelp, checkArgs) where

import Control.Monad ()
import System.Console.GetOpt
  ( ArgDescr (NoArg, ReqArg),
    ArgOrder (Permute),
    OptDescr (..),
    getOpt,
    usageInfo,
  )

data Mode = Text | Binary | Python deriving (Show, Eq)

data Conf = Conf
  { help :: Bool,
    mode :: Maybe Mode,
    input :: Maybe String,
    output :: Maybe String,
    lib :: Maybe String
  }
  deriving (Show, Eq)

defaultConf :: Conf
defaultConf =
  Conf
    { help = False,
      mode = Nothing,
      input = Nothing,
      output = Just "a.out",
      lib = Nothing
    }

checkArgs :: Conf -> Maybe Conf
checkArgs conf =
  if help conf || (validMode (mode conf) && validInput (input conf) && validOutput (output conf))
    then Just conf
    else Nothing

validMode :: Maybe Mode -> Bool
validMode (Just _) = True
validMode Nothing = False

validInput :: Maybe String -> Bool
validInput (Just files) = not (null files)
validInput Nothing = False

validOutput :: Maybe String -> Bool
validOutput (Just _) = True
validOutput Nothing = False

options :: [OptDescr (Conf -> Conf)]
options =
  [ Option ['h'] ["help"] (NoArg (\conf -> conf {help = True})) "Show help message",
    Option ['b'] ["binary"] (NoArg (\conf -> conf {mode = Just Binary})) "Set mode to Binary",
    Option ['t'] ["text"] (NoArg (\conf -> conf {mode = Just Text})) "Set mode to Text",
    Option ['p'] ["python"] (NoArg (\conf -> conf {mode = Just Python})) "Set mode to Python",
    Option ['l'] ["lib"] (ReqArg (\arg conf -> conf {lib = Just arg}) "file") "Set lib directory",
    Option ['s'] ["source"] (ReqArg (\arg conf -> conf {input = Just arg}) "files...") "Input source files",
    Option ['o'] ["output"] (ReqArg (\arg conf -> conf {output = Just arg}) "file") "Output file"
  ]

parseArgs :: [String] -> IO (Maybe Conf)
parseArgs args =
  case getOpt Permute options args of
    (opts, _, []) ->
      (return . checkArgs)
        (foldl (flip id) defaultConf opts)
    _ -> return Nothing

displayHelp :: IO ()
displayHelp = putStrLn $ usageInfo "Usage: ./glados [--text | --binary] [--output | -o] [--source | -s] [--lib | -l]\n" options
