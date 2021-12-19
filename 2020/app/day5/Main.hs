module Main where

import Data.Maybe ( mapMaybe )
import Data.Sort

main :: IO ()
main = do
  input <- input
  putStr "Largest Seat id: "
  print $ maxSeatId input
  putStr "Missing seat: "
  print $ missingSeat $ seatIds input

-- >>> missingSeat [1,2,3,4,6]
-- Just 5
missingSeat :: (Num a, Ord a) => [a] -> Maybe a
missingSeat seats = find sortedSeats
  where
    sortedSeats = sort seats
    find [] = Nothing
    find [s] = Just s
    find (seat1:seat2:seats)
      | abs (seat1 - seat2) > 1 = Just $ seat1 + 1
      | otherwise = find (seat2:seats)

input :: IO [String]
input = lines <$> readFile "inputs/day5.in"

seatIds :: [String] -> [Int]
seatIds = map decodeSeatId

-- >>> maxSeatId
-- 848
maxSeatId :: [String] -> Int
maxSeatId = maximum . seatIds

-- >>> decode [1,0,1]
-- 5
decode :: Num a => [a] -> a
decode values = decode' len values
  where
    len = length values - 1
    decode' (-1) _ = 0
    decode' iter (r:row) = (r * (2 ^ iter)) + decode' (iter - 1) row
    decode' _ _ = 0

-- >>> decodeRow "FBFBBFF"
-- 44
decodeRow :: Num a => [Char] -> a
decodeRow spec = decode rowInfo
  where
    rowInfo = mapMaybe transform spec
    transform a
      | a == 'F' = Just 0
      | a == 'B' = Just 1
      | otherwise = Nothing

-- >>> decodeCol "RLR"
-- 5
decodeCol :: Num a => [Char] -> a
decodeCol spec = decode colInfo
  where
    colInfo = mapMaybe transform spec
    transform a
      | a == 'R' = Just 1
      | a == 'L' = Just 0
      | otherwise = Nothing

-- >>> decodeSeatId "FBFFBBFRLR"
-- 309
decodeSeatId :: [Char] -> Int
decodeSeatId spec = (decodeRow rowInfo * 8) + decodeCol colInfo
  where
    rowInfo = take 7 spec
    colInfo = drop 7 spec


