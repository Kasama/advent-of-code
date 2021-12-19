module Main where

main :: IO ()
main = do
  input <- input
  putStr "First part: "
  print $ head $ targeted input
  putStr "Second part: "
  print $ head $ trioTargeted input

input = readFile "inputs/day1.in"

expenses input = map read $ lines input

target = 2020

-- Exercise one
targeted input = [ a * b | a <- parsedExpenses, b <- parsedExpenses, a + b == target ]
  where
    parsedExpenses = expenses input

-- Exercise two
trioTargeted input = [a * b * c | a <- parsedExpenses, b <- parsedExpenses, c <- parsedExpenses, a + b + c == target]
  where
    parsedExpenses = expenses input
