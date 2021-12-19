module Main where

import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map

inc = (+1)
sub = subtract 1

data Dimension = Dimension { cubes :: Map Coord CubeState, boundaries :: ((Int, Int, Int), (Int, Int, Int)) }

boundChecks :: Dimension -> [Coord]
boundChecks dimension = [ (x, y, z)
                        | x <- [minX..maxX]
                        , y <- [minY..maxY]
                        , z <- [minZ..maxZ]
                        ]
  where ((minX, minY, minZ), (maxX, maxY, maxZ)) = boundaries dimension

nextBounds :: Dimension -> Dimension
nextBounds dim = dim { boundaries = newBounds }
  where
    newBounds = (mapXYZ sub lowBound, mapXYZ inc upBound)
    (lowBound, upBound) = boundaries dim

startingDimension :: [[Char]] -> Dimension
startingDimension chars = Dimension { cubes = Map.empty, boundaries = ((1,1,1), (8,8,1)) }
-- [ "####.#.."
-- , ".......#"
-- , "#..#####"
-- , ".....##."
-- , "##...###"
-- , "#..#.#.#"
-- , ".##...#."
-- , "#...##.."
-- ]

type Coord = (Int, Int, Int)
mapX f (x, y, z) = (f x, y, z)
mapY f (x, y, z) = (x, f y, z)
mapZ f (x, y, z) = (x, y, f z)
mapXYZ f (x, y, z) = (f x, f y, f z)

up, down, left, right, front, back :: Coord -> Coord
up    = mapZ inc
down  = mapZ sub
right = mapY inc
left  = mapY sub
front = mapX inc
back  = mapX sub

neighbors coords = map ($ coords)
  [ up, down, left, right, front, back
  , up . left,           up . right,           up . front,         up . back
  , down . left,         down . right,         down . front,       down . back
  , front . left,        front . right,        back . left,        back . right
  , up . front . left,   up . front . right,   up . back . left,   up . back . right
  , down . front . left, down . front . right, down . back . left, down . back . right
  ]

data CubeState = Active | Inactive
  deriving (Show, Eq)

main = do
  putStr "Hai"

cycle :: Dimension -> Dimension
cycle dimension = dimension

input = readFile "inputs/day17.in"
