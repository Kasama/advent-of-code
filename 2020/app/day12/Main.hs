module Main where

import Text.Parsec
import Control.Monad (void)
import Text.Parsec.String ( Parser )

input = do
  Right instructions <- parse' instructions <$> readFile "inputs/day12.in"
  return instructions

main :: IO ()
main = do
  input <- input
  putStr "Manhatan distance for ferry using wrong method: "
  print $ manhatanDistance $ processAll (startingFerry $ Coord (0,0)) input
  putStr "Manhatan distance for ferry: "
  print $ manhatanDistance $ processAll (startingFerry $ Coord (10,1)) input

number :: Parser Int
number = do
  num <- many digit
  return $ read num

instruction :: Parser Instruction
instruction = do
  s <- oneOf "NSWERLF"
  movement s <$> number
  where
    movement :: Char -> (Int -> Instruction)
    movement s = case s of
                   'N' -> North
                   'S' -> South
                   'W' -> West
                   'E' -> East
                   'R' -> RRight
                   'L' -> RLeft
                   'F' -> Forward

eor :: Parser ()
eor = choice [ void $ try $ char '\n', void eof ]


instructions :: Parser [Instruction]
instructions = many (instruction <* eor)

parse' :: Parser a -> String -> Either ParseError a
parse' p = parse p ""

data Instruction = North Int | South Int | West Int | East Int
                 | RRight Int | RLeft Int
                 | Forward Int
  deriving (Show, Eq)

data Direction = YPositive | XPositive | YNegative | XNegative
  deriving (Show, Eq)
nextDirection direction = case direction of
                            YPositive -> XPositive
                            XPositive -> YNegative
                            YNegative -> XNegative
                            XNegative -> YPositive

data Ferry = Ferry {direction :: Direction, coords :: Coord, waypoint :: Coord}
  deriving (Show, Eq)
startingFerry = Ferry XPositive $ Coord (0, 0)
manhatanDistance Ferry { coords = Coord (x, y)} = abs x + abs y

newtype Coord = Coord (Int, Int)
  deriving (Show, Eq)
clockWiseRotateCoord :: Coord -> Int -> Coord
clockWiseRotateCoord c@(Coord (x, y)) times
  | times == 0 = c
  | otherwise = clockWiseRotateCoord (Coord (y, -x)) (times - 1)

moveWaypoint :: Ferry -> Direction -> Int -> Ferry
moveWaypoint ferry@(Ferry _ _ (Coord (x, y))) direction units = case direction of
                                                                 YPositive -> ferry {waypoint = Coord (x, y + units)}
                                                                 YNegative -> ferry {waypoint = Coord (x, y - units)}
                                                                 XPositive -> ferry {waypoint = Coord (x + units, y)}
                                                                 XNegative -> ferry {waypoint = Coord (x - units, y)}

moveFerry :: Ferry -> Int -> Ferry
moveFerry ferry times = ferry { coords = Coord (x + (wx * times), y + (wy * times)) }
  where
    Coord (x, y) = coords ferry
    Coord (wx, wy) = waypoint ferry

move :: Instruction -> Ferry -> Ferry
move instruction ferry = case instruction of
                           North units -> moveWaypoint ferry YPositive units
                           South units -> moveWaypoint ferry YNegative units
                           East units  -> moveWaypoint ferry XPositive units
                           West units  -> moveWaypoint ferry XNegative units
                           Forward units -> moveFerry ferry units
                           _ -> ferry

rotate :: Instruction -> Ferry -> Ferry
rotate instruction ferry = case instruction of
                             RRight degrees -> rot ferry (times degrees)
                             RLeft degrees -> rot ferry (times (- degrees))
                             _ -> ferry
  where
    times degrees = (4 + (degrees `div` 90)) `mod` 4
    rot ferry clockwiseMoves = ferry { waypoint = clockWiseRotateCoord (waypoint ferry) clockwiseMoves }


process :: Ferry -> Instruction -> Ferry
process ferry instruction = rotate instruction $ move instruction ferry

processAll :: Ferry -> [Instruction] -> Ferry
processAll = foldl process
