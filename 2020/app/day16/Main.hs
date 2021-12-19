module Main where

import Text.Parsec
import Text.Parsec.String 
import Control.Monad ( void )
import Data.List.Split ( splitOn )
import Data.Maybe ( mapMaybe )
import Data.List ( foldl1', isPrefixOf )
import Data.Bifunctor ( Bifunctor(second) )

newtype Constraint = Constraint (Int, Int)
  deriving (Show, Eq)
fitsConstraint :: Int -> Constraint -> Bool
fitsConstraint query (Constraint (lowerBound, upperBound)) = lowerBound <= query && query <= upperBound

data Field = Field { name :: String, constraints :: [Constraint] }
  deriving (Show, Eq)
fitsConstraints :: Int -> Field -> Bool
fitsConstraints query (Field _ constraints) = any (fitsConstraint query) constraints

type Ticket = [Int]

data TicketValidation = Valid
                      | Invalid Int
  deriving (Show, Eq)
toMaybe Valid = Nothing
toMaybe (Invalid i) = Just i
isValid Valid = True
isValid (Invalid _) = False

-- parsers
parse' :: Parser a -> String -> Either ParseError a
parse' p = parse p ""

eol :: Parser ()
eol = void endOfLine <|> eof

number :: Parser Int
number = read <$> many1 digit

fieldName :: Parser String
fieldName = many1 $ letter <|> char ' '

constraintParser :: Parser Constraint
constraintParser = do
  lower <- number
  void $ char '-'
  upper <- number
  return $ Constraint (lower, upper)

ticketParser :: String -> [Int]
ticketParser line = map read $ splitOn "," line

fieldsParser :: Parser [Field]
fieldsParser = many $ do
  name <- fieldName
  void $ string ": "
  constraints <- many1 $ constraintParser <* choice [ try $ void $ string " or ", eol ]
  return $ Field name constraints

yourTicketParser :: Parser Ticket
yourTicketParser = do
  void $ string "your ticket:\n"
  line <- many1 (digit <|> char ',') <* eol
  return $ ticketParser line

nearbyTicketsParser :: Parser [Ticket]
nearbyTicketsParser = do
  void $ string "nearby tickets:\n"
  lines <- many1 $ many1 (digit <|> char ',') <* eol
  return $ map ticketParser lines

-- main
input = readFile "inputs/day16.in"

main = do
  [constraintSection, yourTicketSection, nearbyTicketSession] <- splitOn "\n\n" <$> input
  let Right fields = parse' fieldsParser constraintSection
  let Right ticket = parse' yourTicketParser yourTicketSection
  let Right tickets = parse' nearbyTicketsParser nearbyTicketSession
  putStr "ticket scanning error rate: "
  print $ sum $ mapMaybe (toMaybe . (`validateTicket` fields)) tickets
  let validTickets = filter (isValid . (`validateTicket` fields)) tickets
  putStr "multiply departure field values: "
  print $ multiply $ map (\(i, _) -> ticket !! i) $ departureFields $ reduceConfusion ([], calculateAllPossibleFields validTickets fields)

multiply = foldl1' (*)

valuesAt :: Int -> [Ticket] -> [Int]
valuesAt i = map (!! i)

calculateAllPossibleFields :: [Ticket] -> [Field] -> [(Int, [Field])]
calculateAllPossibleFields tickets fields = map (\(i, v) -> (i, possibleFieldsFor v fields)) values
  where
    values = [ (index - 1, valuesAt (index - 1) tickets) | index <- [1..(length $ head tickets)] ]
    possibleFieldsFor values = filter $ isValid . validateTicket values . pure

reduceConfusion :: ([(Int, Field)], [(Int, [Field])]) -> [(Int, Field)]
reduceConfusion (a, []) = a
reduceConfusion (certain, possible) = reduceConfusion (newCertain, newPossible)
  where
    newCertain = (++) certain $ map (second head) $ filter (\(_, p) -> length p == 1) possible
    isUncertain possibility = possibility `notElem` map snd newCertain
    newPossible = filter (not . null . snd) $ map (second $ filter isUncertain) possible

departureFields :: [(Int, Field)] -> [(Int, Field)]
departureFields = filter (isPrefixOf "departure" . name . snd)

validateTicket :: Ticket -> [Field] -> TicketValidation
validateTicket ticket fields = isTicketValid ticketValidity
  where
    cs = concatMap constraints fields
    valid number = any (fitsConstraint number) cs
    ticketValidity = map (\v -> (v, valid v)) ticket
    isTicketValid [] = Valid
    isTicketValid ((_, True):xs) = isTicketValid xs
    isTicketValid ((value, False):_) = Invalid value
