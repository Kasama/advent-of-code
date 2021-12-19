module Main where

width = length . head

treeChar = '#'

findTrees' :: (Int, Int) -> Int -> [[Char]] -> (Int, Int) -> Int -> Int -> Int
findTrees' _ _ [] _ trees _ = trees
findTrees' moves iter (line:lines) (offsetW, offsetH) trees mapWidth = findTrees' moves nextIter lines (nextOffsetW, nextOffsetH) newTreeSum mapWidth
  where nextIter = iter + 1
        nextOffsetW = if isValidIter then (offsetW + moveLeft) `mod` mapWidth else offsetW
        nextOffsetH = if isValidIter then offsetH + moveDown else offsetH
        (moveLeft, moveDown) = moves
        isValidIter = iter == offsetH
        newTreeSum = if (line !! offsetW) == treeChar && isValidIter then trees + 1 else trees

findTrees :: (Int, Int) -> [[Char]] -> Int -> Int
findTrees moves lines = findTrees' moves 0 lines (0, 0) 0

input = readFile "inputs/day3.in"

main :: IO ()
main = do
  input <- input
  let inputLines = lines input
  let mapWidth = width inputLines
  putStr $ "with movements " ++ show testInput ++ ": "
  print $ res inputLines mapWidth
  putStr "Multiplying: "
  print $ product $ res inputLines mapWidth
  where res inputLines mapWidth = map (\test -> findTrees test inputLines mapWidth) testInput
        testInput = [(1,1), (3,1), (5,1), (7,1), (1,2)]


