module Main where

import Data.Bool(bool)
import Data.List (isPrefixOf)

main :: IO ()
main = do
  input <- input
  putStr "First part: "
  print $ partOne input
  putStr "Second part: "
  print $ partTwo input

input = lines <$> readFile "inputs/day3.in"

mostCommon (a, b) = if a > b then '0' else '1'
leastCommon (a, b) = if a <= b then '0' else '1'

partOne :: [String] -> Integer
partOne input = gammaRate * epsilonRate
  where
    gammaRate = bin2dec $ map mostCommon $ countOccurences input
    epsilonRate = bin2dec $ map leastCommon $ countOccurences input

partTwo :: [String] -> Integer
partTwo input = oxygenRating * co2ScrubberRating
  where
    oxygenRating = bin2dec $ filterBits input mostCommon
    co2ScrubberRating = bin2dec $ filterBits input leastCommon

-- counts the countOccurences of (zeros, ones) in the bits of input
-- resulting list if of the same size as the input length and reports
-- the count of (zeros, ones) in that position
countOccurences :: [String] -> [(Int, Int)]
countOccurences numbers = map (countAll numbers) positions
  where
    inputSize = length $ head numbers
    positions = [0..inputSize-1]
    countAt position (zeros, ones) value =
      if value !! position == '0' then (zeros + 1, ones) else (zeros, ones + 1)
    countAll [] position = (0, 0)
    countAll (value:values) position = countAt position (countAll values position) value

filterBits :: [String] -> ((Int, Int) -> Char) -> String
filterBits = filterBits' 0
  where
    filterBits' :: Int -> [String] -> ((Int, Int) -> Char) -> String
    filterBits' _ [n] _ = n
    filterBits' position numbers criteria =
      filterBits' (position + 1) (filter filterCriteria numbers) criteria
      where
        filterCriteria n = (n !! position) == criteria (countOccurences numbers !! position)

bin2dec :: String -> Integer
bin2dec = foldl (\sum digit -> (2*sum) + (if digit == '0' then 0 else 1)) 0
