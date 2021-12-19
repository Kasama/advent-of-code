module Main where

import Data.List.Split
import Data.Bifunctor (second)
import Math.NumberTheory.Moduli.Chinese

input = lines <$> readFile "inputs/day13.in"

main :: IO ()
main = do
  [earliest, busesString] <- input
  let buses = parseBuses busesString
  putStr "Id of earliest bus times number of mintes waiting: "
  print $ multiplyTimes $ findEarliestBus 0 (read earliest) $ map snd buses
  putStr "Earliest timestamp to win challenge: "
  print $ chineseRemainder buses

parseBuses :: String -> [(Integer, Integer)]
parseBuses busesString = map (second read) $ filter (("x" /=) . snd) $ zip [0,-1..] $ splitOn "," busesString

multiplyTimes (a, b) = map (* a) b

findEarliestBus :: Integer -> Integer -> [Integer] -> (Integer, [Integer])
findEarliestBus waitTime earliestTime buses
  | null onTimeBuses = findEarliestBus (waitTime + 1) earliestTime buses
  | otherwise = (waitTime, onTimeBuses)
  where
    onTimeBuses = filter (\b -> (earliestTime + waitTime) `mod` b == 0) buses
