module Main where

import System.Exit (exitFailure)
import System.IO (readFile)

import Data.Function ((&))
import qualified Data.Text as T
import Text.Printf (printf)

infixr 9 .>
(.>) :: (a -> b) -> (b -> c) -> a -> c
(.>) = flip (.)

inputFile :: FilePath
inputFile = "input.txt"

solvepart1 :: [Int] -> Int
solvepart1 = scanl proc 50 .> filter (==0) .> length
    where 
    proc :: Int -> Int -> Int
    proc a b = ( a + b ) `mod` 100


processpart1 :: [T.Text] -> [Int]
processpart1 = map ( T.strip .> T.unpack .> rotation .> (`mod`100) )
    where 
        rotation ('L':rs) = negate $ ( read rs :: Int )
        rotation ('R':rs) = ( read rs :: Int )
        rotation _ = undefined

solvepart2 :: [Int] -> Int
solvepart2 = scanl proc (50,0) .> map snd .> last
    where 
    proc :: (Int,Int) -> Int -> (Int,Int)
    proc (a1,a2) b = 
        let rot   = b `mod` 100
            sign  = if b < 0 then (-1) else 1
            rem   = ((abs b) `mod` 100) * sign
            count = b & abs .> (`div`100) .> (+a2)
            pass  = a1 /= 0 && ( (a1 + rem) > 99 || (a1 + rem) < 1 )
            r     = (a1 + rot) `mod` 100
        in (r, count + if pass then 1 else 0)


processpart2 :: [T.Text] -> [Int]
processpart2 = map ( T.strip .> T.unpack .> rotation )
    where 
        rotation ('L':rs) = negate ( read rs :: Int )
        rotation ('R':rs) = read rs :: Int
        rotation _ = undefined


main :: IO ()
main = do
    contents <- readFile inputFile
    case lines contents of
        [] -> putStrLn "input.txt is empty!" >> exitFailure
        content -> do
                let solution1 = content & map T.pack .> processpart1 .> solvepart1
                putStrLn $ printf "Part 1: %s" (show solution1)
                let solution2 = content & map T.pack .> processpart2 .> solvepart2
                putStrLn $ printf "Part 2: %s" (show solution2)