module Main where

import Data.Array ((!), (//))
import qualified Data.Array as Array
import Data.List (nub, sort)
import qualified Data.Set as Set

main = do
  input <- input
  putStr "Part one: "
  print $ partOne input

partOne input =
  let hMap = heightMap input
      heights = Array.assocs hMap
      height (_position, height) = height
      totalRiskLevel a = sum $ map (+ 1) a
   in totalRiskLevel $ map height $ lowPoints hMap

lowPoints :: HeightMap -> [(Position, Int)]
lowPoints hMap = filter (isLocalMinima hMap) $ Array.assocs hMap

isLocalMinima :: HeightMap -> (Position, Int) -> Bool
isLocalMinima hMap (pos, v) = all (> v) nValues
  where
    nValues = map (hMap !) $ neighbors hMap pos

input :: IO [[Int]]
input = map (map (read . (: []))) . lines <$> readFile "inputs/day9.in"

testInput :: [[Int]]
testInput =
  [ [2, 1, 9, 9, 9, 4, 3, 2, 1, 0], -- !
    [3, 9, 8, 7, 8, 9, 4, 9, 2, 1], -- !
    [9, 8, 5, 6, 7, 8, 9, 8, 9, 2], -- !
    [8, 7, 6, 7, 8, 9, 6, 7, 8, 9], -- V
    [9, 8, 9, 9, 9, 6, 5, 6, 7, 8] --  y
  ] -- --------------------------> x

type Position = (Int, Int)

type HeightMap = Array.Array Position Int

type Basin = Array.Array Position Int

heightMap :: [[Int]] -> HeightMap
heightMap input =
  let maxX = length $ head input
      maxY = length input
   in Array.array
        ((1, 1), (maxX, maxY))
        [ ((x, y), input !! (y - 1) !! (x - 1))
          | x <- [1 .. maxX],
            y <- [1 .. maxY]
        ]

neighbors :: HeightMap -> Position -> [Position]
neighbors heightMap pos =
  let ((minX, minY), (maxX, maxY)) = Array.bounds heightMap
      (posX, posY) = pos
      inBounds (x, y) =
        x >= minX && x <= maxX
          && y >= minY
          && y <= maxY
   in filter inBounds [(posX + 1, posY), (posX - 1, posY), (posX, posY + 1), (posX, posY - 1)]

findBasin :: HeightMap -> Position -> [Position]
findBasin hMap position = Set.elems $ Set.fromList $ findBasin' initialVisited position
  where
    findBasin' visited position =
      if visited ! position
        then []
        else position : concat [findBasin' (visited // [(position, True)]) neighbor | neighbor <- neighbors hMap position, isInBasin neighbor]
    initialVisited = Array.array (Array.bounds hMap) $ map (\(p, _) -> (p, False)) $ Array.assocs hMap
    isInBasin pos = (hMap ! pos) /= 9
