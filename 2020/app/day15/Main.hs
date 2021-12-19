module Main where

import Data.List.Split
import Data.IntMap (IntMap)
import qualified Data.IntMap as Map
import Data.Foldable

input :: [Int]
input = map read $ splitOn "," "1,0,18,10,19,6"

main = do
  putStr "the 2020th number for the input is: "
  print $ snd $ genNNumbers 2020 input
  putStr "the 30000000th number for the input is: "
  print $ snd $ genNNumbers 30000000 input

processInitial :: [Int] -> IntMap Int
processInitial initial = foldl (\m (i, v) -> Map.insert v i m) Map.empty $ zip [1..] initial

genNNumbers maxRound input = foldl' iterate (initialMem, startingElement) [startingRound..(maxRound - 1)]
  where
    iterate (mem, numToInsert) round = generateNextNumber (round, numToInsert) mem
    startingRound = length input
    startingElement = last input
    initialMem = processInitial input

generateNextNumber :: (Int, Int) -> IntMap Int -> (IntMap Int, Int)
generateNextNumber (turn, element) memory = case (Map.!?) memory element of
                                              Nothing -> insertAndReturn 0
                                              Just index -> insertAndReturn $ turn - index
  where
    insertAndReturn value = (Map.alter (\_ -> Just turn) element memory, value)
