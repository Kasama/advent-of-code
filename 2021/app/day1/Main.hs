module Main where

import qualified Lib.Parser as Parser

main :: IO ()
main = do
  input <- input
  putStr "First part: "
  print $ partOne input
  putStr "Second part: "
  print $ partTwo input

input = map read . lines <$> readFile "inputs/day1.in"

countIncreases input =
    value - 1
    where
    (value, _) = foldl (\(count, prev) next -> (if next > prev then count + 1 else count, next)) (0, 0) input

slidingWindow amount [] = []
slidingWindow amount input =
  sum (take amount input):slidingWindow amount (drop 1 input)

partOne :: (Ord a, Num a) => [a] -> a
partOne = countIncreases

partTwo :: (Ord a, Num a) => [a] -> a
partTwo input = countIncreases $ slidingWindow 3 input
