module Main where

import Lib.Parser
import Text.Parsec ( oneOf, many1, spaces, char, try, many )
import Text.Parsec.String (Parser)
import Data.List.Split (splitOn, splitWhen)
import Data.Either (rights)

main = do
  input <- input
  putStr "Part one: "
  print $ partOne input
  putStr "Part one: "
  print $ partTwo input

partOne :: [String] -> Int
partOne lines = let segments line = last $ splitWhen (== "|") $ splitOn " " line
                in sum $ map (length . filter (\digit -> let segs = length digit
                                                          in segs == 2 -- 1
                                                          || segs == 4 -- 4
                                                          || segs == 3 -- 7
                                                          || segs == 7 -- 8
                                              ) . segments) lines

partTwo lines = let testCases = rights $ map (parse' inputLine) lines
                    finalValues (base, targets) = toDecimal $ map (decodeDisplay base) targets
                in sum $ map finalValues testCases

input = lines <$> readFile "inputs/day8.in"

toDecimal :: [Int] -> Int
toDecimal digits = let digits' = reverse digits
                       toDecimal' _ [] = 0
                       toDecimal' value (x:xs) = (value * x) + toDecimal' (value * 10) xs
                   in toDecimal' 1 digits'

data Display = Display { a :: Bool
                       , b :: Bool
                       , c :: Bool
                       , d :: Bool
                       , e :: Bool
                       , f :: Bool
                       , g :: Bool
                       }
  deriving (Show, Eq)

emptyDisplay :: Display
emptyDisplay = Display { a = False, b = False, c = False, d = False, e = False, f = False, g = False }

countSegments :: Display -> Int
countSegments display = sum $ map (fromEnum . (\f -> f display)) [a, b, c, d, e, f, g]

activeSegments :: Display -> [Display -> Bool]
activeSegments display = filter (\f -> f display) [a, b, c, d, e, f, g]

decodeDisplay :: [Display] -> Display -> Int
decodeDisplay bases toDecode = let segmentTimes seg = sum $ map (fromEnum . seg) bases
                                   segments = countSegments toDecode
                               in case segments of 2 -> 1 -- 1 has 2 segments
                                                   3 -> 7 -- 7 has 3 segments
                                                   4 -> 4 -- 4 has 4 segments
                                                   -- 2, 3 and 5 have 5 segments
                                                   5 -> if any (\s -> segmentTimes s == 6) (activeSegments toDecode)
                                                        then 5 -- has top seg, which apears 6 times in examples
                                                        else if any (\s -> segmentTimes s == 9) (activeSegments toDecode)
                                                             then 3 -- has bot right seg, which apears 9 times in examples
                                                             else 2
                                                   -- 6, 9 and 0 have 6 segments
                                                   6 -> if not (any (\s -> segmentTimes s == 4) (activeSegments toDecode))
                                                        then 9 -- doesn't have bot left seg, which apears 4 times in examples
                                                        else if length (filter (\s -> segmentTimes s == 7) (activeSegments toDecode)) == 1
                                                             then 0 -- has exactly one segment that apear 7 times (bottom)
                                                             else 6 -- has exactly two segments that apear 7 times (mid and bot)
                                                   7 -> 8 -- 8 has 7 segments
                                                   _ -> error "Impossible. Trying to decode display with more than 7 segments" -- other cases are impossible

inputLine :: Parser ([Display], [Display])
inputLine = do
  baseDisplays <- many1 (display <* spaces)
  char '|' <* spaces
  targetDisplays <- many1 (display <* try spaces)
  many $ char '\n'
  return (baseDisplays, targetDisplays)

display :: Parser Display
display = do
  value <- many1 $ oneOf "abcdefg"
  return $ foldl (\display c -> case c of 'a' -> display {a = True}
                                          'b' -> display {b = True}
                                          'c' -> display {c = True}
                                          'd' -> display {d = True}
                                          'e' -> display {e = True}
                                          'f' -> display {f = True}
                                          'g' -> display {g = True}
                 ) emptyDisplay value
