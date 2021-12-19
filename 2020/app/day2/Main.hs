module Main where

import Text.Parsec
import Text.Parsec.String
import Control.Monad (void)

data InputLine = InputLine { firstConstraint :: Int
                           , secondConstraint :: Int
                           , lookupLetter :: Char
                           , password :: [Char]
                           }

regularParse :: Parser a -> String -> Either ParseError a
regularParse p = parse p ""

inputLineParser :: Parser InputLine
inputLineParser = do
  minValue <- many1 digit
  void $ char '-'
  maxValue <- many1 digit
  void $ char ' '
  item <- anyChar
  void $ string ": "
  pass <- many1 anyChar
  return $ InputLine { firstConstraint = read minValue, secondConstraint = read maxValue, lookupLetter  = item, password = pass }

parseLine :: [Char] -> Either ParseError InputLine
parseLine = regularParse inputLineParser

policyOne :: Either ParseError InputLine -> Bool
policyOne (Left _) = False
policyOne (Right line) = firstConstraint line <= passLen && secondConstraint line >= passLen
  where passLen = length $ filter (\c -> c == lookupLetter line) $ password line

policyTwo :: Either ParseError InputLine -> Bool
policyTwo (Left _) = False
policyTwo (Right line) = first /= second
  where get f = password line !! (f line - 1) == lookupLetter line
        first = get firstConstraint
        second = get secondConstraint

parsedLines = map parseLine

inputs = lines <$> readFile "inputs/day2.in"

main :: IO ()
main = do
  input <- inputs
  putStr "Number of passwords following policy one: "
  print $ length $ filter policyOne $ parsedLines input
  putStr "Number of passwords following policy two: "
  print $ length $ filter policyTwo $ parsedLines input

