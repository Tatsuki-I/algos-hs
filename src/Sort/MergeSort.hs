module Sort.MergeSort where

import Data.List

mergeSort :: Ord a => [a] -> [a]
mergeSort [] = []
mergeSort
