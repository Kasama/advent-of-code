module Main where

import Data.Maybe ( isJust )

-- First number to break the pattern:
-- >>> findWrongInStream 25 . getStream <$> input
-- Just 26796446

-- Encryption Weakness
-- >>> (flip getEncryptionWeakness) 26796446 <$> getStream <$> input
-- Just 3353494

input = readFile "inputs/day9.in"

main :: IO ()
main = do
  input <- input
  let stream = getStream input
  putStr "first number to break the pattern: "
  let weaknessTarget = findWrongInStream 25 stream
  print weaknessTarget
  putStr "encryption weakness is: "
  print $ getEncryptionWeakness stream =<< weaknessTarget

getStream :: String -> [Int]
getStream input = map read (lines input)

preambleSize :: Int
preambleSize = 5

testInput :: [Int]
testInput = [ 35 , 20 , 15 , 25 , 47 , 40 , 62 , 55 , 65 , 95 , 102
            , 117 , 150 , 182 , 127 , 219 , 299 , 277 , 309 , 576 ]


find :: Eq t => t -> [t] -> Maybe t
find _ [] = Nothing
find needle (item:haystack)
  | needle == item = Just item
  | otherwise = find needle haystack

twoSumExists :: [Int] -> Int -> Bool
twoSumExists [] _ = False
twoSumExists (item:preamble) key
  | isJust $ find (key - item) preamble = True
  | otherwise = twoSumExists preamble key

getAllEncryptionWeaknessess :: Int -> [Int] -> [Maybe [Int]]
getAllEncryptionWeaknessess target stream = [ findSumFrom (drop i stream) target | i <- [0..(length stream)] ]

-- >>> getEncryptionWeakness testInput 127
-- Just 62
getEncryptionWeakness :: [Int] -> Int -> Maybe Int
getEncryptionWeakness [] _ = Nothing
getEncryptionWeakness stream target
  | isJust foundSum = do sum <- foundSum
                         return $ minimum sum + maximum sum
  | otherwise = getEncryptionWeakness (tail stream) target
  where
    foundSum = findSumFrom stream target

-- >>> findSumFrom (drop 2 testInput) 127
-- Just [15,25,47,40]
findSumFrom :: [Int] -> Int -> Maybe [Int]
findSumFrom stream target = findSumFrom' 2 stream target
  where
    findSumFrom' iter stream target
      | iter > length stream = Nothing
      | currentSum > target = Nothing
      | currentSum == target = Just $ take iter stream
      | currentSum < target = findSumFrom' (iter + 1) stream target
      where
        currentSum = sum (take iter stream)

-- >>> findWrongInStream preambleSize testInput
-- Just 127
findWrongInStream preambleSize stream
  | length stream <= preambleSize = Nothing
  | twoSumExists (take preambleSize stream) (stream !! preambleSize) = findWrongInStream preambleSize (tail stream)
  | otherwise = Just $ stream !! preambleSize
