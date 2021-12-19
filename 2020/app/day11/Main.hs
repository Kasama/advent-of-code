module Main where

import Data.Array
import Data.List.Split
import Data.Maybe
import Debug.Trace

newtype Board = Board (Array Coord Char)
  deriving (Eq)
instance Show Board where
  show = showSeatMap

newtype Coord = Coord (Int, Int)
  deriving (Eq, Ord, Ix, Show)

input :: IO [[Char]]
input = lines <$> readFile "inputs/day11.in"

main :: IO ()
main = do
  input <- input
  putStr "Number of occupied seats after stabilization (neighbors): "
  print $ count isOccupied $ processUntilStable (makeBoard input) Neighbors
  putStr "Number of occupied seats after stabilization (in sight): "
  print $ count isOccupied $ processUntilStable (makeBoard input) InSight

testInput = [ "L.LL.LL.LL"
            , "LLLLLLL.LL"
            , "L.L.L..L.."
            , "LLLL.LL.LL"
            , "L.LL.LL.LL"
            , "L.LLLLL.LL"
            , "..L.L....."
            , "LLLLLLLLLL"
            , "L.LLLLLL.L"
            , "L.LLLLL.LL" ]

within :: Coord -> (Int, Int) -> Bool
within (Coord (x, y)) (boundX, boundY)
  | x > boundX = False
  | x < 0      = False
  | y > boundY = False
  | y < 0      = False
  | otherwise  = True

inc = (+) 1
dec = subtract 1
-- directions
up, down, left, right, upLeft, upRight, downLeft, downRight :: (Int -> Int, Int -> Int)
up        = (id , dec)
down      = (id , inc)
left      = (dec, id )
right     = (inc, id )
upLeft    = (dec, dec)
upRight   = (inc, dec)
downLeft  = (dec, inc)
downRight = (inc, inc)

go (Coord (x, y)) (directionX, directionY) = Coord (directionX x, directionY y)

data Operation = InSight | Neighbors
  deriving (Eq)

inSight :: Board -> Coord -> (Int, Int) -> [Coord]
inSight (Board seatMap) coord bounds = catMaybes linesOfSight
  where
    linesOfSight = [ search up coord
                   , search down coord
                   , search left coord
                   , search right coord
                   , search upLeft coord
                   , search upRight coord
                   , search downLeft coord
                   , search downRight coord
                   ]
    search direction initialCoord
      | inBounds && (occupied || free) = Just current
      | inBounds = search direction current
      | otherwise = Nothing
      where
        inBounds = current `within` bounds
        occupied = isOccupied $ seatMap ! current
        free = isFree $ seatMap ! current
        current = go initialCoord direction

-- >>> neighborsOf (3,9) (9,9)
-- [(2,9),(4,9),(3,8),(4,8),(4,8)]
neighborsOf :: Coord -> (Int, Int) -> [Coord]
neighborsOf (Coord (x, y)) (boundX, boundY)
  | (x,y) == (0,0)            = map Coord [(x+1, y), (x, y+1), (x+1, y+1)]
  | (x,y) == (boundX, boundY) = map Coord [(x-1, y), (x, y-1), (x-1, y-1)]
  | (x,y) == (0, boundY)      = map Coord [(x+1, y), (x, y-1), (x+1, y-1)]
  | (x,y) == (boundX, 0)      = map Coord [(x-1, y), (x, y+1), (x-1, y+1)]
  | x == 0                    = map Coord [(x, y-1), (x, y+1), (x+1, y), (x+1, y+1), (x+1, y-1)]
  | x == boundX               = map Coord [(x, y-1), (x, y+1), (x-1, y), (x-1, y+1), (x-1, y-1)]
  | y == 0                    = map Coord [(x-1, y), (x+1, y), (x, y+1), (x+1, y+1), (x-1, y+1)]
  | y == boundY               = map Coord [(x-1, y), (x+1, y), (x, y-1), (x+1, y-1), (x-1, y-1)]
  | otherwise                 = map Coord [(x+1, y), (x, y+1), (x-1, y), (x, y-1), (x+1, y+1), (x+1, y-1), (x-1, y+1), (x-1, y-1)]

-- >>> (makeBoard testInput) ! (0,9)
-- 'L'
makeBoard :: [[Char]] -> Board
makeBoard list = Board $ array (Coord (0,0), Coord (boundY, boundX))
                          [ (Coord (y, x), (list !! y) !! x)
                          | x <- [0..boundX]
                          , y <- [0..boundY] ]
  where
    boundX = length (head list) - 1
    boundY = length list - 1

printSeatMap seatMap = putStr $ showSeatMap seatMap

showSeatMap :: Board -> String
showSeatMap (Board seatMap) = unlines $ chunksOf size $ elems seatMap
  where
    size = length [lowerBound..upperBound]
    (Coord (lowerBound, _), Coord (_, upperBound)) = bounds seatMap

count :: (Char -> Bool) -> Board -> Int
count f (Board list) = length $ filter f $ elems list

-- >>> unlines $ chunksOf 10 $ elems $ processUntilStable (makeBoard testInput)
-- "#.##.##.##\n##L####.##\n#.#.#..#..\n##L#.##.##\n#.##.##.##\n#.#####.##\n..#.#.....\n##L#L#####\n#.#LLLL#.#\n#.#####.##\n"
processUntilStable :: Board -> Operation -> Board
processUntilStable board op
  | board == nextIteration = board
  | otherwise = processUntilStable nextIteration op
  where
    nextIteration = processBoard board op

processBoard :: Board -> Operation -> Board
processBoard board@(Board seatMap) op = Board $ seatMap // [ (s, process board s op) | s <- indices seatMap ]

isFree s     = s == 'L'
isOccupied s = s == '#'

process :: Board -> Coord -> Operation -> Char
process board@(Board seatMap) seat op
  | isFree (getSeat seat)     && numberOfOccupiedSeats == 0 = '#'
  | isOccupied (getSeat seat) && numberOfOccupiedSeats >= minOccupied = 'L'
  | otherwise = seatMap ! seat
  where
    minOccupied = if op == InSight then 5 else 4
    numberOfOccupiedSeats = length $ filter (isOccupied .getSeat) seats
      where
        seats = if op == InSight then sight
                else neighbors
    getSeat s = seatMap ! s
    neighbors    = neighborsOf seat (upperBoundX, upperBoundY)
    sight = inSight board seat (upperBoundX, upperBoundY)
    (_, Coord (upperBoundX, upperBoundY)) = bounds seatMap
