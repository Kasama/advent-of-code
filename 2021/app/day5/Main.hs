module Main where

import qualified Data.Map as Map
import Control.Monad (void)
import Text.Parsec hiding (Line)
import Text.Parsec.String (Parser)
import Lib.Parser

main = do
  lines <- parseInput (many line) input
  putStr "Part One: "
  print . countAbove 2 . paintLines $ cardinalLines lines
  putStr "Part Two: "
  print . countAbove 2 . paintLines $ lines

input = readFile "inputs/day5.in"

line :: Parser Line
line = do
  x1 <- number; char ','; y1 <- number
  string " -> "
  x2 <- number; char ','; y2 <- number
  eor
  return $ Line (Point (x1, y1), Point (x2, y2))

newtype Point = Point (Int, Int)
  deriving (Show, Eq)

newtype Line = Line (Point, Point)
  deriving (Show, Eq)


isCardinal :: Line -> Bool
isCardinal line = isHorizontal line || isVertical line
  where
    isHorizontal (Line (Point (x1, _), Point (x2, _))) = x1 == x2
    isVertical (Line (Point (_, y1), Point (_, y2))) = y1 == y2

cardinalLines :: [Line] -> [Line]
cardinalLines = filter isCardinal

paintLines :: [Line] -> Map.Map (Int, Int) Int
paintLines = foldl paintLine Map.empty
  where
    paintLine b l@(Line (Point (x1, y1), Point (x2, y2))) =
      if isCardinal l then
        upsert b [((x, y), b ! (x, y) + 1) | x <- range x1 x2, y <- range y1 y2]
      else
        upsert b [(k, b ! k + 1) | k <- zip (range x1 x2) (range y1 y2)]
    b ! pos = Map.findWithDefault 0 pos b
    upsert b [] = b
    upsert b ((k, v):xs) = Map.insert k v (upsert b xs)

range :: (Ord a, Num a, Enum a) => a -> a -> [a]
range a b
  | a > b = [a, a-1 .. b]
  | otherwise = [a..b]

countAbove :: Ord a => a -> Map.Map (Int, Int) a -> Int
countAbove value arr = length $ filter (>= value) (Map.elems arr)
