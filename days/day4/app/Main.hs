module Main where

import Control.Parallel.Strategies (parMap, rdeepseq)

import Data.Array

import Data.Function ((&))
import Data.Foldable (foldr1, foldr')

import System.Exit (exitFailure)
import System.IO (readFile)

infixr 9 .>
(.>) :: (a -> b) -> (b -> c) -> a -> c
(.>) = flip (.)

type Grid = Array (Int,Int) Char

-- | Filter by neighbor count
filterNeighbors :: Bool -> Int -> Grid -> Int
filterNeighbors r n grid =  
    let ( o, m ) = bounds grid
        next     = assocs grid & parMap rdeepseq (check n)
        start    = grid & assocs .> filter (snd .> (=='x')) .> length
        run      = array (o,m) .> filterNeighbors r n
        total    = next & filter (snd .> (=='x')) & length
    in if r && total > start
        then run next
        else total
    where
    neighbors (x,y) ( (ox,oy), ( bx, by) ) n = 
        [ (x',y') | 
          px <- [-1,0,1]
        , py <- [-1,0,1]
        , let x'=x+px
        , let y'=y+py
        , x' >= ox
        , x' <= bx
        , y' >= oy
        , y' <= by
        , (x',y') /= (x,y) ]
    isRoll '@' = 1
    isRoll _   = 0 
    check n ( coord, v ) | v == '@'   = neighbors coord (bounds grid) n & map ( (grid!) .> isRoll ) & sum & (\s -> ( coord, if s < n then 'x' else '@' ))
                         | otherwise  = ( coord, v )

mkGrid :: [String] -> Grid
mkGrid c = 
    let grid          = c & map ( zip [1..] ) .> zip [1..] .> map proc .> concat
        ( (w,h ), _ ) = last grid 
    in  array ((1, 1), (w, h)) grid
    where
    proc (rn,cs) = map (\(cn,v) -> ((cn,rn),v)) cs


inputFile :: FilePath
inputFile = "input.txt"

main :: IO ()
main = do
    contents <- readFile inputFile
    case lines contents of
        [] -> putStrLn "Input is empty." >> exitFailure
        grid  -> do
            putStr "Part 1: " >> (grid & mkGrid .> filterNeighbors False 4 .> print)
            putStr "Part 2: " >> (grid & mkGrid .> filterNeighbors True 4 .> print )
