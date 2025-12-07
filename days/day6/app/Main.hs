module Main where

import Control.Parallel.Strategies (parMap, rdeepseq, rpar)

import Data.Function ((&))
import Data.List (break,foldr1,foldl',sortBy, scanl', transpose)

import System.Exit (exitFailure)
import System.IO (readFile)

infixr 9 .>
(.>) :: (a -> b) -> (b -> c) -> a -> c
(.>) = flip (.)

inputFile :: FilePath
inputFile = "input.txt"

data Input = Input [[Integer]] [(Integer -> Integer -> Integer, Integer)]

process1 :: [String] -> Input
process1 cs = 
    let ( p : ps ) = cs & map words .> reverse
        ops        = map getOp p
    in map (map read) ps `Input` ops
    where
    getOp "*" = ( (*), 1 )
    getOp "+" = ( (+), 0 )
    getOp _   = undefined

solve1 :: Input -> Integer
solve1 (Input numbers problems) = go problems numbers & map snd .> sum
    where
    cmb (f, x) y   = (f, x `f` y)
    go ps (xs:xss) = go (zipWith cmb ps xs) xss
    go ps []       = ps

data Input2 = Input2 [Integer] [(Integer -> Integer -> Integer, Integer)]

process2 :: [String] -> Input2
process2 cs = 
    let ( p : ps ) = reverse cs
        ops        = p & words .> map getOp
        flipped    = ps & transpose .> map reverse
        numbers    = flipped & map strip .> map toNum
    in numbers `Input2` ops
    where
    toNum [] = 0
    toNum n  = (read n :: Integer)
    strip = filter (/=' ')
    getOp "*" = ( (*), 1 )
    getOp "+" = ( (+), 0 )
    getOp _   = undefined

solve2 :: Input2 -> Integer
solve2 (Input2 numbers ps@((_,ident):_)) = foldl' go ([],ident,ps) numbers & (\(a,s,_) -> s:a) .> sum
    where
    go (s,n,pt@((p,i):ps)) x | x == 0,  ((_,n'):_) <- ps     = (n:s,n', ps)
                             | otherwise                     = let n' = n`p`x in (s,n', pt)

main :: IO ()
main = do
    contents <- readFile inputFile
    case lines contents of
        [] -> putStrLn "Input is empty." >> exitFailure
        s  -> do
            let input1 = process1 s
            let input2 = process2 s
            putStr "Part 1: " >> print (solve1 input1)
            putStr "Part 2: " >> print (solve2 input2)
