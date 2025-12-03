{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Parallel (par, pseq)
import Control.Parallel.Strategies (parMap, rpar)
import Control.DeepSeq (NFData)

import Data.Function ((&))
import Data.List (concat)
import Data.Text (Text, pattern Empty, pattern (:<), pattern (:>))
import qualified Data.Text as T

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

recursivePolishPalindrome :: T.Text -> Bool
recursivePolishPalindrome = go ""
    where
    go _   Empty                                     = False
    go as' t@( a :< as ) | as' == t                  = True
                         | T.length as' > T.length t = False
                         | otherwise                 = let prefix      = as' `T.isPrefixOf` t
                                                           ( las, lt ) = ( T.length as', T.length t )
                                                           fits        = lt `rem` las == 0
                                                           ct          = lt `div` las
                                                           expand      = las > 0 && prefix && fits && ( take ct (repeat as') & T.concat .> (==t) )
                                                           recur       = go ( T.snoc as' a ) as
                                                       in expand `par` ( recur `pseq` ( expand || recur ) )

polishPalindrome :: T.Text -> Bool
polishPalindrome = go ""
    where
    go _   Empty                                     = False
    go as' t@( a :< as ) | as' == t                  = True
                         | T.length as' > T.length t = False
                         | otherwise                 = go ( T.snoc as' a ) as

solvePart1 :: [(Integer,Integer)] -> Integer
solvePart1 = ( parMap rpar ( ranges .> map showText .> filter polishPalindrome .> map readTextInteger ) ) .> concat .> sum
    where
    ranges (a, b) | a < b     = [a..b]
                  | otherwise = undefined

solvePart2 :: [(Integer,Integer)] -> Integer
solvePart2 = ( parMap rpar ( ranges .> map showText .> filter recursivePolishPalindrome .> map readTextInteger ) ) .> concat .> sum
    where
    ranges (a, b) | a < b     = [a..b]
                  | otherwise = undefined

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
