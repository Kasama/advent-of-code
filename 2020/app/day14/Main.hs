module Main where

import Numeric
import Data.Char
import Text.Parsec
import Text.Parsec.String
import Control.Monad ( void )
import qualified Data.IntMap as Map
import Debug.Trace

input = readFile "inputs/day14.in"

main = do
  input <- input
  let Right prog = parse program "" input
  putStr "Sum of all mem values: "
  let finalMemV1 = processV1 prog initialMem
  print $ sum $ memData finalMemV1
  putStr "Sum of all mem values for v2: "
  let finalMemV2 = processV2 prog initialMem
  print $ sum $ memData finalMemV2

data Instruction = Mask String
                 | Mem Int Int
  deriving (Show, Eq, Ord)

type Program = [Instruction]

data Memory = Memory { memData :: Map.IntMap Int, currentMask :: String }
  deriving (Show)
initialMem = Memory { memData = Map.empty, currentMask = "" }

---------------- parser
number :: Parser Int
number = read <$> many1 digit

eq :: Parser String
eq = string " = "

mem :: Parser Instruction
mem = do
  void $ string "mem["
  addr <- number
  void $ string "]"
  void eq
  val <- number
  void endOfLine <|> eof
  return $ Mem addr val

mask :: Parser Instruction
mask = do
  void $ string "mask"
  void eq
  val <- many1 $ letter <|> digit
  void endOfLine <|> eof
  return $ Mask val

instruction :: Parser Instruction
instruction = choice [ try mem, try mask ]

program :: Parser Program
program = many instruction

------ End parser


-- >>> toBin 8
-- [1,0,0,0]
toBin :: Int -> [Int]
toBin num = map digitToInt $ showIntAtBase 2 intToDigit num ""

-- >>> toDec $ toBin 8
-- 8
toDec :: [Int] -> Int
toDec bin = foldr (\x y -> x + 2*y) 0 $ reverse bin

-- >>> padTo 64 $ toBin 182793
padTo :: Int -> a -> [a] -> [a]
padTo len def bin = prelude ++ bin
  where
    preludeLength = len - length bin
    prelude = if preludeLength > 0 then replicate preludeLength def else []

-- >>> toDec $ applyMaskV1 "t" $ toBin 72649
-- 72649
applyMaskV1 :: [Char] -> [Int] -> [Int]
applyMaskV1 mask bin = zipWith bitwiseMask paddedMask paddedBin
  where
    paddedMask = padTo (length bin) 'X' mask
    paddedBin = padTo (length mask) 0 bin
    bitwiseMask c i
      | c == '0' = 0
      | c == '1' = 1
      | otherwise = i

-- >>> processV1 [Mask "XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X", Mem 8 11, Mem 7 101, Mem 8 0] initialMem
-- Memory {memData = fromList [(7,101),(8,64)], currentMask = "XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X"}
processV1 :: Program -> Memory -> Memory
processV1 [] memory = memory
processV1 ((Mask mask):program) memory = processV1 program $ memory {currentMask = mask}
processV1 ((Mem addr val):program) memory = processV1 program patchedMemory
  where
    patchedMemory = memory { memData = Map.alter update addr (memData memory) }
    update _ = Just $ toDec $ applyMaskV1 (currentMask memory) $ toBin val

-- >>> applyMaskV2 "00000000000000000000000000000001001X" $ toBin 12
-- [[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,0],[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1]]
applyMaskV2 :: [Char] -> [Int] -> [[Int]]
applyMaskV2 mask bin = map reverse $ foldl rebuild [[]] $ zipWith bitwiseMask paddedMask paddedBin
  where
    paddedMask = padTo (length bin) 'X' mask
    paddedBin = padTo (length mask) 0 bin
    bitwiseMask c i
      | c == '0' = [i]
      | c == '1' = [1]
      | otherwise = [0,1]
    rebuild [] addition = rebuild [[]] addition
    rebuild existing addition = concatMap (\e -> map (: e) addition) existing

processV2 :: Program -> Memory -> Memory
processV2 [] memory = memory
processV2 ((Mask mask):program) memory = processV2 program $ memory {currentMask = mask}
processV2 ((Mem addr val):program) memory = processV2 program patchedMemory
  where
    patchedMemory = memory { memData = foldl (flip $ Map.alter update) (memData memory) addrs }
    addrs = map toDec $ applyMaskV2 (currentMask memory) $ toBin addr
    update _ = Just val
