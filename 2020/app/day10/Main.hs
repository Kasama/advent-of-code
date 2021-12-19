module Main where

-- import Lib.Graph
import Data.Graph
import Data.List
import qualified Data.Vector as Vector

-- Graph helers
data CGraph node a = CGraph { graph :: Graph
                            , getNode :: Vertex -> (node, a, [a])
                            , getVertex :: a -> Maybe Vertex
                            }
instance Show (CGraph a b) where
  show cgraph = show $ graph cgraph
toCGraph (a, b, c) = CGraph { graph = a, getNode = b, getVertex = c}

-- >>> (multOneThree . findJoltDifferences) <$> input
-- 1856

-- >>> possibleArrangements <$> input
-- 2314037239808
main :: IO ()
main = do
  input <- input
  putStr "number of 1-jolts multiplied by the number of 3-jolts: "
  print $ multOneThree $ findJoltDifferences input
  putStr "number of possible transformer combinations: "
  print $ possibleArrangements input

input :: IO [Int]
input = fmap read . lines <$> readFile "inputs/day10.in"

testInput :: [Int]
testInput = [ 28 , 33 , 18 , 42 , 31 , 14 , 46 , 20 , 48 , 47 , 24 , 23 , 49 , 45 , 19
            , 38 , 39 , 11 , 1 , 32 , 25 , 35 , 8 , 17 , 7 , 9 , 4 , 2 , 34 , 10 , 3 ]

multOneThree :: Num a => (a, b, a) -> a
multOneThree (a, _, b) = a * b

-- >>> findJoltDifferences smallExample
-- (7,0,5)
findJoltDifferences transformers = calcJolts initialJolts sorted
  where
    initialJolts
      | value == 1 = (1, 0, 0)
      | value == 2 = (0, 1, 0)
      | value == 3 = (0, 0, 1)
      | otherwise  = (0, 0, 0)
      where
        value = head sorted
    sorted = sort transformers
    calcJolts jolts [] = jolts
    calcJolts (ones, twos, threes) [_] = calcJolts (ones, twos, threes + 1) []
    calcJolts (ones, twos, threes) (a:b:others)
      | difference == 1 = calcJolts (ones + 1, twos, threes) next
      | difference == 2 = calcJolts (ones, twos + 1, threes) next
      | difference == 3 = calcJolts (ones, twos, threes + 1) next
      | otherwise       = calcJolts (ones, twos, threes) next
      where
        next = b:others
        difference = abs $ a - b

-- >>> possibleArrangements (testInput)
-- 19208
possibleArrangements :: [Int] -> Int
possibleArrangements values = Vector.head pathsAccumulator
  where
    allTransformerValues = 0:values
    amountOfTransformers = length allTransformerValues
    valuesVector = Vector.fromList (sort allTransformerValues)
    pathsAccumulator = Vector.generate amountOfTransformers countArrangements
    countArrangements index
      | index == amountOfTransformers - 1 = 1
      | otherwise = sum ((pathsAccumulator Vector.!) <$> indices)
      where
        indices = [ (index + 1)..(index + length validNexts) ]
        validNexts = Vector.takeWhile isValidNextTransformer currentValues
        currentValues = Vector.drop (index + 1) valuesVector
        isValidNextTransformer val = abs (val - (valuesVector Vector.! index)) <= 3
