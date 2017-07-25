module Sort.QuickSort where

import           Data.List

quickSort        :: Ord a => [a] -> [a]
quickSort []     =  []
quickSort (x:xs) =  quickSort lesser ++ x : quickSort greater
  where
    (lesser, greater) = partition (< x) xs
