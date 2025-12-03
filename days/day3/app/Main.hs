module Main where

import Control.Parallel.Strategies (parMap, rpar)

import Data.Function ((&))
import Data.Foldable (foldr')

import System.Exit (exitFailure)
import System.IO (readFile)

infixr 9 .>
(.>) :: (a -> b) -> (b -> c) -> a -> c
(.>) = flip (.)

inputFile :: FilePath
inputFile = "input.txt"

solvePart :: Int -> [[Integer]] -> Integer
solvePart n = parMap rpar (solve n) .> sum
    where
    solve n xs = let d         = length xs - n
                     (s, left) = foldl' go ([], d) xs
                  in s & drop left .> reverse .> map show .> concat .> read
    go (s:ss,  d) n | d > 0 && s < n = go (ss, d-1) n
    go (stack, d) n                  = (n:stack, d)

main :: IO ()
main = do
    contents <- readFile inputFile
    case lines contents of
        [] -> putStrLn "Input is empty." >> exitFailure
        cs  -> do
            let banks = cs & map ( map (:[]) .> map read )
            let solution1 = banks & solvePart 2
            putStr "Part 1: " >> print solution1
            let solution2 = banks & solvePart 12
            putStr "Part 2: " >> print solution2
