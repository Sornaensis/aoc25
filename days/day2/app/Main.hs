{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Parallel (par, pseq)
import Control.Parallel.Strategies (parMap, rpar, parList, rdeepseq, using)
import Control.DeepSeq (NFData)

import Data.Foldable (foldr')
import Data.Function ((&))
import Data.List.Split (chunksOf)
import Data.List (concat, intercalate,scanl')
import Data.Text (Text, pattern Empty, pattern (:<), pattern (:>))
import qualified Data.Text as T
import qualified Data.Set as Set

import System.Exit (exitFailure)
import System.IO (readFile)

infixr 9 .>
(.>) :: (a -> b) -> (b -> c) -> a -> c
(.>) = flip (.)

readTextInteger :: T.Text -> Integer
readTextInteger = T.unpack .> read

readText :: Read a => T.Text -> a
readText = T.unpack .> read

showText :: Show a => a -> T.Text
showText = show .> T.pack

processRanges :: T.Text -> [(Integer,Integer)]
processRanges = T.splitOn "," .> map ( T.splitOn "-" .> tuple )
    where
    tuple [a,b] = ( readText a :: Integer, readText b :: Integer )
    tuple _     = undefined

inputFile :: FilePath
inputFile = "input.txt"

solvePart1 :: [(Integer,Integer)] -> Integer
solvePart1 = rangesM False & solve

solvePart2 :: [(Integer,Integer)] -> Integer
solvePart2 = rangesM True & solve

solve :: ((Integer, Integer) -> [[Integer]]) -> [(Integer,Integer)] -> Integer
solve f rs = rs & parMap rdeepseq f .> concat .> concat .> Set.fromList .> Set.toList .> sum

rangesM :: Bool -> (Integer, Integer) -> [[Integer]]
rangesM split (a, b) =  let  ( la, lb ) = (length . show $ a, length . show $ b)
                             range      = [la..lb]
                        in range & parMap rpar gen & concat
    where
    iterate x n     = let ct         = x`div`n
                          max        = repeat 9 & take (T.length start) .> map showText .> T.concat .> readTextInteger
                          sequence   = take ct $ repeat (10^(n-1))
                          startNum   = readTextInteger start
                          start      = sequence & map showText .> foldr' (<>) ""
                          inc        = start & T.reverse .> readTextInteger
                          rs         = [startNum,startNum+inc..b]
                          inRange n  =  n >= startNum && n <= max && n >= a && n <= b
                      in if ct < 2 
                            then [] 
                            else 
                                filter inRange rs               
    gen    1        = []
    gen    n        = let chunks = if split 
                                    then filter ((n`mod`) .> (==0)) [1..n] 
                                    else [n] & filter even .> map (`div`2)
                          results = parMap rpar (iterate n) chunks
                      in  results `using` parList rdeepseq

main :: IO ()
main = do
    contents <- readFile inputFile
    case lines contents of
        [content]  -> do
            let solution1 = content & T.pack .> processRanges .> solvePart1
            putStr "Part 1: " >> print solution1
            let solution2 = content & T.pack .> processRanges .> solvePart2
            putStr "Part 2: " >> print solution2
        []         -> putStrLn "Input is empty." >> exitFailure
        _          -> putStrLn "Input is invalid." >> exitFailure
