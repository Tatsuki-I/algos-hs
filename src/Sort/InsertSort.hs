module Sort.InsertSort where

import Data.List

insertSort :: Ord a => [a] -> [a]
insertSort []       = []
insertSort (x : xs) = insert x $ insertSort xs
