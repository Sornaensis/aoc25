module Main where

import Control.Parallel.Strategies (parMap, rdeepseq)

import Data.Array

import Data.Function ((&))
import Data.Foldable (foldr1, foldr')

import qualified Data.Set as S

import System.Exit (exitFailure)
import System.IO (readFile)

infixr 9 .>
(.>) :: (a -> b) -> (b -> c) -> a -> c
(.>) = flip (.)

type Grid = Array (Int,Int) Char
type Found = S.Set (Int,Int)
type Rolls  = S.Set (Int,Int)

-- | Filter by neighbor count
filterNeighbors :: Bool -> Int -> Grid -> Int
filterNeighbors r n grid = filterNeighbors' r n grid (assocs grid & filter (snd .> (=='@')) .> map fst .> S.fromList) (S.empty)

filterNeighbors' :: Bool -> Int -> Grid -> Rolls -> Found -> Int
filterNeighbors' r n grid elems found = 
    let ( o, m ) = bounds grid
        start    = S.size found
        next     = S.toList elems & map (\c -> ( c, grid!c ) ) .> parMap rdeepseq (check n) .> filter snd .> map fst .> S.fromList .> S.union found
        elems'   = (S.difference elems next)
        run      = filterNeighbors' r n grid elems'
        total    = S.size next
    in if r && total > start
        then run next
        else total
    where
    neighbors (x,y) ( ( ox, oy ), ( bx, by) ) n = 
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
    find c     | c `S.member` found = 0
    find c                          = c & (grid!) .> isRoll
    check n ( coord, v ) | v == '@'   = neighbors coord (bounds grid) n & map find & sum & (\s -> ( coord, s < n ))
                         | otherwise  = ( coord, False )

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
            let input = grid & mkGrid
            putStr "Part 1: " >> ( input & filterNeighbors False 4 .> print)
            putStr "Part 2: " >> ( input & filterNeighbors True 4 .> print )
