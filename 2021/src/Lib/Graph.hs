module Lib.Graph ( CGraph
                 , toCGraph
                 , module Data.Graph
                 ) where

import Data.Graph

data CGraph node a = CGraph { graph :: Graph
                            , getNode :: Vertex -> (node, a, [a])
                            , getVertex :: a -> Maybe Vertex
                            }
instance Show (CGraph a b) where
  show cgraph = show $ graph cgraph
toCGraph (a, b, c) = CGraph { graph = a, getNode = b, getVertex = c}
