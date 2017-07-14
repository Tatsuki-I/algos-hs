module Sort.QuickSort where

import Data.List

quickSort :: Ord a => [a] -> [a]
quickSort [] = []
quickSort (x : xs) = quickSort smaller ++ quickSort larger
    where
        smaller = filter (<=x) xs
        larger = filter (>x) xs
