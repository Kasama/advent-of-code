module Main where

import Text.Parsec
    ( char,
      digit,
      oneOf,
      string,
      choice,
      many1,
      many,
      parse,
      try,
      ParseError )
import Text.Parsec.String ( Parser )
import Control.Monad ( void )
import Data.Array ( (!), (//), bounds, listArray, Array )
import Data.Set ( insert, member, Set, empty )

data Instruction = Noop Int
                 | Acc Int
                 | Jmp Int
  deriving (Show, Eq, Read)

data ProgramState = InfLoop
                  | Finished Int
  deriving (Show, Eq)
isFinished InfLoop = False
isFinished (Finished _) = True

type Program = Array Int Instruction
mkProgram instructions = listArray (0, length instructions - 1) instructions

simpleParse :: Parser a -> String -> Either ParseError a
simpleParse parser = parse parser ""

-- >>> simpleParse operation "acc -2"
-- Right (Acc (-2))
instruction :: Parser (Int -> Instruction)
instruction = choice [ Noop <$ try (string "nop")
                     , Acc  <$ try (string "acc")
                     , Jmp  <$      string "jmp"
                     ]
              <* char ' '

-- >>> simpleParse argument "+82"
-- Right 82
argument :: Parser Int
argument = do
  sign <- oneOf "+-"
  value <- many1 digit
  let signal = if sign == '+' then "" else [sign]
  return $ read $ signal ++ value

operation :: Parser Instruction
operation = do
  op <- instruction
  op <$> argument

program :: Parser [Instruction]
program = many $ do
  op <- operation
  void $ many1 $ oneOf " \n\t"
  return op

testInput = "nop +0\nacc +1\njmp +4\nacc +3\njmp -3\nacc -99\nacc +1\njmp -4\nacc +6\n"

-- >>> parsedTestInput
-- [Noop 0,Acc 1,Jmp 4,Acc 3,Jmp (-3),Acc (-99),Acc 1,Jmp (-4),Acc 6]
parsedTestInput = v
  where
    Right v = parse program "" testInput

input = readFile "inputs/day8.in"

-- Executes function moving program counter and updating the accumulator if necessary
execute f pc acc newVisitedLines program = case program ! pc of
  Noop _ -> f (pc + 1) acc newVisitedLines program
  Acc val -> f (pc + 1) (acc + val) newVisitedLines program
  Jmp val -> f (pc + val) acc newVisitedLines program

-- >>> run 0 0 empty (listArray (0, length parsedTestInput) parsedTestInput)
-- 5
run :: Int -> Int -> Set Int -> Array Int Instruction -> Int
run pc acc visitedLines instructions
  | member pc visitedLines = acc
  | otherwise              = execute run pc acc newVisitedLines instructions
  where
    newVisitedLines = insert pc visitedLines

-- >>> checkProgramState 0 0 empty $ mkProgram parsedTestInput
-- Finished 2
checkProgramState :: Int -> Int -> Set Int -> Program -> ProgramState
checkProgramState pc acc visitedLines program
  | programOver = Finished acc
  | foundLoop   = InfLoop
  | otherwise   = execute checkProgramState pc acc newVisitedLines program
  where
    programOver = pc < min || pc >= max
      where (min, max) = bounds program
    foundLoop   = programOver || member pc visitedLines
    newVisitedLines = insert pc visitedLines

flipInstruction :: Instruction -> Instruction
flipInstruction (Noop a) = Jmp a
flipInstruction (Acc a) = Acc a
flipInstruction (Jmp a) = Noop a

-- >>> map (checkProgramState 0 0 empty) $ genVariations $ mkProgram parsedTestInput
-- [InfLoop,InfLoop,InfLoop,InfLoop,InfLoop,InfLoop,InfLoop,Finished 2,InfLoop]
genVariations :: Program -> [Program]
genVariations program = [program // [(i, flipped i)] | i <- [loBound..hiBound]]
  where
    flipped i = flipInstruction $ program ! i
    (loBound, hiBound) = bounds program

main :: IO ()
main = do
  input <- input
  putStr "Accumulator before first repeated instruction: "
  let Right parsedInput = parse program "" input
  print $ run 0 0 empty $ mkProgram parsedInput
  putStr "Accumulator after program ended: "
  print $ filter isFinished $ map (checkProgramState 0 0 empty) $ genVariations $ mkProgram parsedInput




