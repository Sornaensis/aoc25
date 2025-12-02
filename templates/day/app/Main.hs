module Main where

import System.Exit (exitFailure)
import System.IO (readFile)

inputFile :: FilePath
inputFile = "input.txt"

main :: IO ()
main = do
    contents <- readFile inputFile
    case lines contents of
        [] -> putStrLn "Input is empty." >> exitFailure
        _  -> do
            putStrLn "Part 1: <todo>"
            putStrLn "Part 2: <todo>"
