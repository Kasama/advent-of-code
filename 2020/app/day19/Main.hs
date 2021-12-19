module Main where

import Data.Array
import Data.List.Split
import Data.Either
import Control.Monad
import Text.Parsec
import Text.Parsec.String

main = do
  putStr "Hello"

input :: IO ([String], [String])
input = do
  raw <- readFile "inputs/day19.in"
  let [rules, messages] = splitOn "\n\n" raw
  return (lines rules, lines messages)

testRules = [ "0: 4 1 5"
            , "1: 2 3 | 3 2"
            , "2: 4 4 | 5 5"
            , "3: 4 5 | 5 4"
            , "4: \"a\""
            , "5: \"b\""
            ]

type Parsers = Array Int (Parser String)

parsers :: [String] -> Parsers
parsers rules = array (0, length rules - 1) (rights $ map (parse' rule) rules)

parse' :: Parser a -> String -> Either ParseError a
parse' p = parse p ""

number :: Parser Int
number = read <$> many1 digit

-- >>> parse' (unwrap $ parse' (recursiveRule) "2 3 4 1 | 7 6") "2341"
-- Right "2341"

-- >>> parse' (unwrap $ parse' (letterRule) "\"a\"") "a"
-- Right "a"

unwrap (Right a) = a

letterRule :: Parser (Parsers -> Parser String)
letterRule = (\s _ -> string s) <$> between (char '"') (char '"') (many1 letter)

-- >>> parse' (unwrap $ parse' (recursiveRuleFragment) "2 3 5 1") "2351"
-- Right "2351"

recursiveRuleFragment :: Parser (Parsers -> Parser String)
recursiveRuleFragment = do numbers <- many1 (number <* spaces)
                           let nextParserLoader arr = return . concat <$> thread $ map ((\f -> f arr) . flip (!)) numbers
                           -- return (\_ -> string "hello")
                           return nextParserLoader

recursiveRuleFragment' :: Parser (Parser String)
recursiveRuleFragment' = do numbers <- thread . (map string . (:[])) <$> many1 (digit <* spaces)
                            return $ concat <$> numbers

rulesSeparator :: Parser ()
rulesSeparator = (void (char '|') <|> eof) <* spaces

recursiveRule :: Parser (Parsers -> Parser String)
recursiveRule = aux2
  where
    aux arr = map (\f -> try $ f arr) <$> many1 (recursiveRuleFragment <* rulesSeparator)
    aux2 arr = do t <- aux arr
                  return $ choice t

-- >>> parse' (thread $ map void [char 'a', char 'b', char 'c']) "abc"
-- Right ()
thread :: [Parser a] -> Parser [a]
thread [] = return []
thread (parser:parsers) = do parsed <- parser
                             others <- thread parsers
                             return $ parsed : others

-- exprz :: Parser (Parser String)
-- exprz = letterRule <|> recursiveRule

rule :: Parser (Int, Parser String)
rule = do
  n <- number
  void $ string ":"
  void spaces
  -- parsers <- many $ expr <* (void (char '|') <|> eof) <* spaces
  let p = (:[]) <$> char 'b'
  return (n, p)
