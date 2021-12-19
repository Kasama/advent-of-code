module Main where

import Text.Parsec
import Text.Parsec.String
import Control.Monad ( void )
import Data.Graph
import Debug.Trace

data Bag = Bag Int Color
  deriving (Show)
instance Eq Bag where
  Bag _ x == Bag _ y = x == y
instance Ord Bag where
  compare (Bag _ x) (Bag _ y) = compare x y
amountOf (Bag x _) = x
colorOf (Bag _ (Color c)) = c

newtype Color = Color String
  deriving (Show, Eq, Ord)

data CGraph node a = CGraph { graph :: Graph
                            , getNode :: Vertex -> (node, a, [a])
                            , getVertex :: a -> Maybe Vertex
                            }
instance Show (CGraph a b) where
  show cgraph = show $ graph cgraph
toCGraph (a, b, c) = CGraph { graph = a, getNode = b, getVertex = c}


whitespace :: Parser String
whitespace = many $ oneOf "\n\t, "

simpleParse :: Parser a -> String -> Either ParseError a
simpleParse parser = parse parser ""

testParse parser = simpleParse (many $ parser <* whitespace)

-- >>> testParse number "8 9"
-- Right [8,9]
number :: Parser Int
number = do
  num <- many1 digit
  return $ read num

-- >>> testParse eol ".\n\n"
-- Right [()]
eol :: Parser ()
eol = void $ char '.' <* whitespace

-- >>> testParse (bag) "bags bags"
-- Right ["bags","bags"]
bag :: Parser String
bag = choice [ try $ string "bags", try $ string "bag" ]

-- >>> testParse color "muted red batata azul"
-- Right [Color "muted red",Color "batata azul"]
color :: Parser Color
color = do
  modifier <- many1 letter
  void whitespace
  colorName <- many1 letter
  return $ Color $ modifier ++ " " ++ colorName

contain :: Parser String
contain = string "contain"

-- >>> testParse bags "2 dark bronze bags, 1 muted bronze bag"
-- Right [[Bag 2 (Color "dark bronze"),Bag 1 (Color "muted bronze")]]
bags :: Parser [Bag]
bags = many1 $ do
  num <- number
  void whitespace
  col <- color
  void whitespace
  void bag
  void whitespace
  return $ Bag num col

noBags :: Parser [Bag]
noBags = do
  void $ string "no other bags"
  return []

-- >>> parse (many line) "" "dotted salmon bags contain 2 dark lavender bags, 1 muted red bag, 1 vibrant magenta bag."
-- Right [(Bag 1 (Color "dotted salmon"),[Bag 2 (Color "dark lavender"),Bag 1 (Color "muted red"),Bag 1 (Color "vibrant magenta")])]
line :: Parser (Bag, [Bag])
line = do
  b <- color
  void whitespace
  void bag
  void whitespace
  void contain
  void whitespace
  insideBags <- choice [ try noBags, bags]
  void eol
  return (Bag 1 b, insideBags)

inputFile = readFile "inputs/day7.in"

exampleInput :: [Char]
exampleInput = "dotted bronze bags contain 2 dark bronze bags, 1 muted bronze bag.\nshinny bronze bag contain 2 dotted bronze bags."

findConnectionsTo cgraph key = map (getNode cgraph) $ reachable transposed vertex
  where
    transposed = transposeG $ graph cgraph
    Just vertex = getVertex cgraph key

-- >>> makeGraph (parseInput exampleInput)
-- array (0,1) [(0,[]),(1,[0])]
makeGraph :: [(Bag, [Bag])] -> CGraph Bag String
makeGraph edges = cgraph
  where
    cgraph = toCGraph $ graphFromEdges graphEdges
    transform (bag, innerBags) = (bag, colorOf bag, map colorOf innerBags)
    graphEdges = map transform edges

-- >>> parseInput exampleInput
-- [(Bag 1 (Color "dotted bronze"),[Bag 2 (Color "dark bronze"),Bag 1 (Color "muted bronze")]),(Bag 1 (Color "shinny bronze"),[Bag 2 (Color "dotted bronze")])]
parseInput :: String -> [(Bag, [Bag])]
parseInput input = case parse (many line) "" input of
                    Left _ -> []
                    Right parsed -> parsed

-- cgraph = makeGraph $ parseInput exampleInput

vert cgraph vertez = vertex
  where
    Just vertex = getVertex cgraph vertez

-- >>> graph cgraph
-- array (0,1) [(0,[]),(1,[0])]

-- >>> dfs (graph cgraph) [vert "muted bronze"]
-- /home/roberto/documents/programming/advent-of-code/kasama/2020/day7/src/Lib.hs:128:5-41: Non-exhaustive patterns in Just vertex

targetBag = "shiny gold"

bagsWithin :: CGraph Bag String -> Vertex -> Int
bagsWithin cgraph vertex = bagsWithin' spanningTree
  where
    [spanningTree] = dfs (graph cgraph) [vertex]
    bagsWithin' :: Tree Vertex -> Int
    bagsWithin' (Node bagV innerBags) = trace ("bag " ++ show (getNode cgraph bagV) ++ " has \t" ++ show ret ++ " bags within") (ret)
      where
        fst (a, _, _) = a
        ret = foldl countBags 1 innerBags
        countBags :: Int -> Tree Vertex -> Int
        countBags acc (Node eachVertex subTrees) = acc + (amount * sum ( map bagsWithin' subTrees))
          where
            (Bag amount _, _, _) = getNode cgraph eachVertex

getBag :: CGraph Bag String -> Vertex -> String
getBag cgraph vertex = "" ++ show thisBag ++ " => " ++ show bagsWithin
  where
    bagsWithin = map (getNode cgraph . vert cgraph) keyBagsWithin
    (thisBag, _, keyBagsWithin) = getNode cgraph vertex

main :: IO ()
main = do
  input <- inputFile
  let cgraph = makeGraph $ parseInput input
  putStr "Amount of bags that can contain 'shiny gold' is: "
  print $ (+) (-1) $ length $ map (\(_, a, _) -> a) $ findConnectionsTo cgraph targetBag
  let bags = bagsWithin cgraph $  vert cgraph targetBag
  putStr "Amount of bags needed within a 'shiny gold' is: "
  print bags
  putStr $ foldl (\s t -> s ++ show (getBag cgraph t) ++ "\n\n\n") "" (head $ dfs (graph cgraph) [vert cgraph targetBag])




