module Main where

import Text.Parsec
import Text.Parsec.String
import Control.Monad
import Data.Either
import Data.List.Split (splitOn)

data Passport = Passport
                 { byr :: Bool , iyr :: Bool , eyr :: Bool , hgt :: Bool
                 , hcl :: Bool , ecl :: Bool , pid :: Bool
                 } deriving (Show)

data Token = TByr | TIyr | TEyr | THgt
           | THcl | TEcl | TPid
  deriving (Show)

emptyPassport = Passport False False False False False False False

addToPassport :: Passport -> Token -> Passport
addToPassport passport TByr = passport { byr = True }
addToPassport passport TIyr = passport { iyr = True }
addToPassport passport TEyr = passport { eyr = True }
addToPassport passport THgt = passport { hgt = True }
addToPassport passport THcl = passport { hcl = True }
addToPassport passport TEcl = passport { ecl = True }
addToPassport passport TPid = passport { pid = True }

isValid :: Passport -> Bool
isValid p = all (\f -> f p) [byr, iyr, eyr, hgt, hcl, ecl, pid]

-- Part 1
value :: Parser String
value = many1 $ digit <|> letter <|> char '#'

colon = char ':'

existanceParser :: Parser Token
existanceParser = choice
  [ TByr <$ try (string "byr" <* colon <* value)
  , TIyr <$ try (string "iyr" <* colon <* value)
  , TEyr <$ try (string "eyr" <* colon <* value)
  , THgt <$ try (string "hgt" <* colon <* value)
  , THcl <$ try (string "hcl" <* colon <* value)
  , TEcl <$ try (string "ecl" <* colon <* value)
  , TPid <$ try (string "pid" <* colon <* value)
  ]

-- Part 2
yearBetween :: Int -> Int -> Parser Int
yearBetween min max = do
  num <- count 4 digit
  guard (min <= read num && read num <= max)
  return $ read num

heightCm :: Parser Int
heightCm = do
  num <- count 3 digit
  string "cm"
  guard (150 <= read num && read num <= 193)
  return $ read num

heightIn :: Parser Int
heightIn = do
  num <- count 2 digit
  string "in"
  guard (59 <= read num && read num <= 76)
  return $ read num

height :: Parser Int
height = do
  choice
    [ try heightIn
    , try heightCm
    ]

hexColor :: Parser [Char]
hexColor = do
  char '#'
  count 6 $ oneOf "0123456789abcdef"

eyeColor :: Parser [Char]
eyeColor = do
  choice $
    map (try . string)
    [ "amb", "blu", "brn", "gry", "grn", "hzl", "oth" ]

validationParser :: Parser Token
validationParser = choice
  [ TByr <$ try (string "byr" <* colon <* yearBetween 1920 2002 <* eof)
  , TIyr <$ try (string "iyr" <* colon <* yearBetween 2010 2020 <* eof)
  , TEyr <$ try (string "eyr" <* colon <* yearBetween 2020 2030 <* eof)
  , THgt <$ try (string "hgt" <* colon <* height <* eof)
  , THcl <$ try (string "hcl" <* colon <* hexColor <* eof)
  , TEcl <$ try (string "ecl" <* colon <* eyeColor <* eof)
  , TPid <$ try (string "pid" <* colon <* count 9 digit <* eof)
  ]

checkPassport :: Parser Token -> [Char] -> Bool
checkPassport parser line = isValid passport
  where
    passport = foldl addToPassport emptyPassport parsed
    parsed = rights $ map (parse parser "") (splitOn " " line)

input = readFile "inputs/day4.in"
inputs = splitOn "\n\n"

main :: IO ()
main = do
  input <- input
  let lines = map (unwords . splitOn "\n") $ inputs input
  putStr "Passports with all fields: "
  print $ countValid $ allFields lines
  putStr "Passports with valid fields: "
  print $ countValid $ validated lines
  -- print lines
  where
    countValid list = length $ filter (== True) list
    checkWith parser input = map (checkPassport parser) input
    allFields input = checkWith existanceParser input
    validated input = checkWith validationParser input


