--
-- EPITECH PROJECT, 2024
-- Glados [WSL: Ubuntu]
-- File description:
-- vm
--

import System.Console.GetOpt
import System.Environment
import ParseInstructions(readAndParseInstructionFile)
import ExecuteInstructions(executeInstructions)
import Builtins.PrintStack(printStack)

data Options = Options
    { optBinary     :: Bool
    , optInstruction :: Bool
    , optHelp       :: Bool
    , optFilePath   :: Maybe String
    } deriving Show

defaultOptions :: Options
defaultOptions = Options
    { optBinary     = False
    , optInstruction = False
    , optHelp       = False
    , optFilePath   = Nothing
    }

options :: [OptDescr (Options -> Options)]
options =
    [ Option ['b'] ["binary"]
        (NoArg (\opts -> opts { optBinary = True }))
        "Run in binary mode"
    , Option ['t'] ["instruction"]
        (NoArg (\opts -> opts { optInstruction = True }))
        "Run in instruction mode"
    , Option ['h'] ["help"]
        (NoArg (\opts -> opts { optHelp = True }))
        "Show help"
    ]

main :: IO ()
main = do
    args <- getArgs
    let (actions, nonOptions, _) = getOpt RequireOrder options args
        opts = foldl (flip id) defaultOptions actions

    case (optHelp opts, nonOptions, optBinary opts, optInstruction opts) of
        (True, _, _, _) -> printHelp
        (_, filePath:_, True, _) -> runBinaryMode filePath
        (_, filePath:_, _, True) -> runInstructionMode filePath
        _ -> putStrLn "Invalid arguments" >> printHelp

printHelp :: IO ()
printHelp = putStrLn $ usageInfo header options
    where header = "Usage: myvm [OPTION...] files..."

runBinaryMode :: String -> IO ()
runBinaryMode filePath = putStrLn $ "Running in binary mode with file: " ++ filePath

runInstructionMode :: String -> IO ()
runInstructionMode filePath = do
    putStrLn $ "Running in instruction mode with file: " ++ filePath
    instructions <- readAndParseInstructionFile filePath
    let finalStack = executeInstructions instructions [] 0  -- Pass an empty stack and start from instruction 0
    printStack finalStack

