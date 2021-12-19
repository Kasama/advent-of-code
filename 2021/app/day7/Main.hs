module Main where

import Data.List.Split (splitOn)
import Lib.Math (median)
import Lib.Parser (parseInput, number)

main = do
  input <- input
  putStr "Part One: "
  print $ fuelCost linearTarget linearDistance input
  putStr "Part Two: "
  print $ partTwo input

input :: IO [Integer]
input = map read . splitOn "," <$> readFile "inputs/day7.in"

linearTarget :: [Integer] -> Integer
linearTarget positions = ceiling $ median (map fromInteger positions)

linearDistance :: Num a => a -> a -> a
linearDistance a b = abs $ a - b

fuelCost :: ([Integer] -> Integer) -> (Integer -> Integer -> Integer) -> [Integer] -> Integer
fuelCost target distance positions = foldl (\acc p -> (+) acc $ distance (target positions) p) 0 positions

partTwo :: [Integer] -> Integer
partTwo input =
  let range = [minimum input..maximum input]
      target = const
      triangularDistance a b = n * (n + 1) `div` 2
        where n = linearDistance a b
  in minimum $ map (\t -> fuelCost (target t) triangularDistance input) range
