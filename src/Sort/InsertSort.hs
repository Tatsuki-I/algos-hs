module Sort.InsertSort where

import Data.List

insertSort :: Ord a => [a] -> [a]
insertSort []   = []
insertSort list = foldr insert [] list
