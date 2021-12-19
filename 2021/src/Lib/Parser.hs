module Lib.Parser where

import Control.Monad (void)
import Text.Parsec
import Text.Parsec.String (Parser)

parseInput :: Parser b -> IO String -> IO b
parseInput p input = do
  Right parsed <- parse' p <$> input
  return parsed

parse' :: Parser a -> String -> Either ParseError a
parse' p = parse p ""

eor :: Parser ()
eor = choice [ void $ try $ char '\n', void eof ]

number :: Parser Int
number = do
  num <- spaces *> many1 digit
  return $ read num
