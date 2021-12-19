module Main where

import Control.Monad (void)
import Data.Array
import Text.Parsec
import Text.Parsec.String (Parser)
import Lib.Parser

main :: IO ()
main = do
  input <- input
  putStr "First part: "
  putStr "Second part: "

input = readFile "inputs/day4.in"

numbersLine :: Parser [Int]
numbersLine = many1 (number <* optional (oneOf ", "))

endOfBoard :: Parser ()
endOfBoard = void $ eor <* eor

board :: Parser Board
board = do
  _ <- many (space <|> newline)
  lines <- many1 numbersLine
  try eor
  return $
    Board
      { size = length lines,
        numbers =
          array
            ((1, 1), (length lines, length lines))
            [ ((row, col), lines !! (row - 1) !! (col - 1))
              | row <- [1 .. length lines],
                col <- [1 .. length lines]
            ],
        marked =
          array
            ((1, 1), (length lines, length lines))
            [ ((row, col), False)
              | row <- [1 .. length lines],
                col <- [1 .. length lines]
            ]
      }

boards :: Parser [Board]
boards = many (board <* optional eor)

inputParser :: Parser [Board]
inputParser = do
  numbers <- numbersLine
  many1 board

type BingoNumbers = [Int]

data Board = Board {size :: Int, numbers :: Array (Int, Int) Int, marked :: Array (Int, Int) Bool}
  deriving (Show, Eq)

markBoard :: Int -> Board -> Board
markBoard number board = case findNumber (numbers board) number of
  Just index -> board {marked = marked board // [(index, True)]}
  Nothing -> board
  where
    findNumber arr searchValue = case filter (\(i, val) -> val == searchValue) (assocs arr) of
                                   (found:_) -> Just $ fst found
                                   _ -> Nothing

isWinnerBoard :: Board -> Bool
isWinnerBoard board =
  any (any isFull . (\f -> f $ marked board)) [lines, columns, diagonals]
  where
    columnBound = snd . snd . bounds
    rowBound = fst . snd . bounds
    isFull = all (== True)
    lines arr =
      [ [arr ! (row, col) | col <- [1 .. columnBound arr]]
        | row <- [1 .. rowBound arr]
      ]
    columns arr =
      [ [arr ! (row, col) | row <- [1 .. rowBound arr]]
        | col <- [1 .. columnBound arr]
      ]
    diagonals arr =
      [ [arr ! (n, n) | n <- [1 .. rowBound arr]], -- main diagonal
        [ arr ! (row, col) -- antidiagonal
          | row <- [1 .. rowBound arr],
            col <- [1 .. columnBound arr],
            row + col == 1 + rowBound arr
        ]
      ]
