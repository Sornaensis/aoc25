module Main where

import Control.Parallel.Strategies (parMap, rdeepseq, rpar)

import Data.Function ((&))
import Data.List (break,foldr1,foldl',sortBy)
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
    check n = parMap rpar ($n) checkRanges & or

solve2 :: Input -> Integer
solve2 (Input ranges _) =
    let sorted = sortBy cmp ranges
    in collect sorted & count
    where
    cmp (a,_) (c,_) = a `compare` c
    combine n (a,b) = n + (b-a+1)
    count = foldl' combine 0
    collect []                = []
    collect ((a,b):(c,d):rs)  | c <= b, d' <- max b d  = collect $ (a,d') : rs
                              | otherwise              = (a,b) : collect ( (c,d) : rs )
    collect rs                = rs

main :: IO ()
main = do
    contents <- readFile inputFile
    case lines contents of
        [] -> putStrLn "Input is empty." >> exitFailure
        cs  -> do
            let input = cs & process
            let solution1 = solve1 input
            putStr "Part 1: " >> print solution1
            let solution2 = solve2 input
            putStr "Part 2: " >> print solution2
