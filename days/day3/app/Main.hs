module Main where

import Control.Parallel (par, pseq)
import Control.Parallel.Strategies (parMap, rpar, parList, rdeepseq, using)
import Control.DeepSeq (NFData)

import Data.Function ((&))
import Data.Foldable (foldr')

import System.Exit (exitFailure)
import System.IO (readFile)

infixr 9 .>
(.>) :: (a -> b) -> (b -> c) -> a -> c
(.>) = flip (.)

inputFile :: FilePath
inputFile = "input.txt"

solvePart1 :: [[Integer]] -> Integer
solvePart1 = parMap rpar solve .> sum
    where
    cmp   e@(n,ix)  (a,ix') | a > n     = (a,ix')
                            | otherwise = e
    solve xs = let bs       = xs & flip zip [1..]
                   size     = length bs
                   pos      = if fx == size then (/=fx) else (>fx)
                   (f, fx)  = bs & foldr' cmp (-1,-1)
                   (s, sx)  = bs & filter (snd .> pos) .> foldr' cmp (-1,-1)
                in  if fx < sx 
                        then f * 10 + s 
                        else s * 10 + f

main :: IO ()
main = do
    contents <- readFile inputFile
    case lines contents of
        [] -> putStrLn "Input is empty." >> exitFailure
        cs  -> do
            let banks = cs & map ( map (:[]) .> map read )
            let solution1 = solvePart1 banks
            putStr "Part 1: " >> print solution1
