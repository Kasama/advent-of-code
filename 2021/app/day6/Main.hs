module Main where

import Control.Monad (void)
import Data.List (group, sort)
import qualified Data.Map as Map
import Text.Parsec hiding (Line)
import Text.Parsec.String (Parser)
import Lib.Parser (parseInput, number)

main = do
  fish <- parseInput school input
  putStr "Part One: "
  print . countSchool $ simulate 6 2 80 fish
  putStr "Part Two: "
  print . countSchool $ simulate 6 2 256 fish

eor :: Parser ()
eor = choice [void $ try $ char '\n', void $ try $ char ',', void eof]

school :: Parser School
school = do
  fishes <- many $ number <* eor
  return $ foldl (\school f -> Map.insert (head f) (length f) school) Map.empty (group $ sort fishes)

input = readFile "inputs/day6.in"

type Fish = Int

type School = Map.Map Fish Int

countSchool :: School -> Int
countSchool = Map.foldl (+) 0

simulate :: Int -> Int -> Int -> School -> School
simulate lifespan birthPenalty days school = foldl (\school _ -> simulate' school) school [1 .. days]
  where
    simulate' school = Map.insert lifespan (Map.findWithDefault 0 0 school + Map.findWithDefault 0 lifespan (agedSchool school)) (agedSchool school)
    agedSchool school = Map.mapKeys (\age -> if age == 0 then lifespan + birthPenalty else age - 1) school
    lifes = [0..lifespan+birthPenalty]
