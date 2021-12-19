module Lib.Math where

import Data.List (sort)

-- | Compute the median of a list

median :: (Ord a, Fractional a) => [a] -> a
median x =
   if odd n
     then sort x !! (n `div` 2)
     else ((sort x !! (n `div` 2 - 1)) + (sort x !! (n `div` 2))) / 2
    where n = length x
