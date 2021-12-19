module Main where

import Text.Parsec
import Text.Parsec.String
import Data.Either

main = do
  input <- homework
  putStr "Sum of homework lines without precedence: "
  print $ sum $ rights $ map (parse' samePrecedenceExpr) input
  putStr "Sum of homework lines with addition before multiplication: "
  print $ sum $ rights $ map (parse' addBeforeMultiply) input

homework = lines <$> readFile "inputs/day18.in"

parse' :: Parser a -> String -> Either ParseError a
parse' p = parse p ""

number :: Parser Int
number = read <$> many1 digit

parens :: Parser a -> Parser a
parens = between (char '(') (char ')')

betweenWhitespace :: Parser a -> Parser a
betweenWhitespace = between spaces spaces

samePrecedenceExpr :: Parser Int
samePrecedenceExpr = chainl1 (betweenWhitespace $ parens samePrecedenceExpr <|> number) (add <|> multiply)

addBeforeMultiply :: Parser Int
addBeforeMultiply = chainl1 (chainl1 (betweenWhitespace $ parens addBeforeMultiply <|> number) add) multiply
  where betweenWhitespace = between spaces spaces

multiply :: Parser (Int -> Int -> Int)
multiply = (*) <$ char '*'

add :: Parser (Int -> Int -> Int)
add = (+) <$ char '+'
