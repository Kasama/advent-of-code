module Main where

import Control.Monad (void)
import Text.Parsec
import Text.Parsec.String (Parser)
import Lib.Parser ( eor, number, parseInput )

main :: IO ()
main = do
  putStr "First part: "
  instructions <- parseInput instructions input
  print $ partOne instructions
  putStr "Second part: "
  print $ partTwo instructions

partOne instructions = position submarine * depth submarine
  where
    submarine = runPartOneInstructions (Submarine {position = 0, depth = 0, aim = 0}) instructions

partTwo instructions = position submarine * depth submarine
  where
    submarine = runPartTwoInstructions (Submarine {position = 0, depth = 0, aim = 0}) instructions

input = readFile "inputs/day2.in"

data Instruction = Forward Int | Down Int | Up Int
  deriving (Show, Eq)

data Submarine = Submarine {depth :: Int, position :: Int, aim :: Int}
  deriving (Show, Eq)

instruction :: Parser Instruction
instruction = do
  s <- choice [string "forward", string "down", string "up"]
  movement s <$> number
  where
    movement :: String -> (Int -> Instruction)
    movement s = case s of
      "forward" -> Forward
      "down" -> Down
      "up" -> Up

instructions :: Parser [Instruction]
instructions = many (instruction <* eor)


runPartOneInstructions :: Submarine -> [Instruction] -> Submarine
runPartOneInstructions = foldl runInstruction
  where
    runInstruction :: Submarine -> Instruction -> Submarine
    runInstruction sub (Forward i) = sub {position = position sub + i}
    runInstruction sub (Up i)      = sub {depth = depth sub - i}
    runInstruction sub (Down i)    = sub {depth = depth sub + i}

runPartTwoInstructions :: Submarine -> [Instruction] -> Submarine
runPartTwoInstructions = foldl runInstruction
  where
    runInstruction :: Submarine -> Instruction -> Submarine
    runInstruction sub (Forward i) = sub {position = position sub + i, depth = depth sub + (aim sub * i)}
    runInstruction sub (Up i)      = sub {aim = aim sub - i}
    runInstruction sub (Down i)    = sub {aim = aim sub + i}
