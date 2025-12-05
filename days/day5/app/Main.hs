module Main where

import Control.Parallel.Strategies (parMap, rdeepseq)

import Data.Function ((&))
import Data.List (break,foldr1)
import Data.List.Split (splitOn)

import System.Exit (exitFailure)
import System.IO (readFile)

infixr 9 .>
(.>) :: (a -> b) -> (b -> c) -> a -> c
(.>) = flip (.)

inputFile :: FilePath
inputFile = "input.txt"

data Input = Input [(Integer,Integer)] [Integer]

process :: [String] -> Input
process ss = 
    let ( ranges, (_:ids) ) = break (=="") ss
    in map (splitOn "-" .> getRange) ranges `Input` map read ids
    where
    getRange [l,h] = (read l,read h)
    getRange _     = undefined

solve1 :: Input -> Int
solve1 (Input ranges ids) = ids & parMap rdeepseq check .> filter id .> length
    where
    mkCheck :: (Integer,Integer) -> (Integer -> Bool)
    mkCheck (a,b) = \c -> c >= a && c <= b
    checkRanges :: [Integer -> Bool]
    checkRanges = ranges & map mkCheck 
    check n = parMap rdeepseq ($n) checkRanges & or

main :: IO ()
main = do
    contents <- readFile inputFile
    case lines contents of
        [] -> putStrLn "Input is empty." >> exitFailure
        cs  -> do
            let solution1 = cs & process .> solve1
            putStr "Part 1: " >> print solution1
            putStrLn "Part 2: <todo>"
