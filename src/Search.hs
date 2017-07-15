module Search where

import Sort.QuickSort

sequentialSearch :: Eq a => [a] -> a -> Bool
sequentialSearch [] _ = False
sequentialSearch (x : xs) target
        | x == target = True
        | otherwise   = sequentialSearch xs target

isSorted :: Ord a => [a] -> Bool
isSorted []  = True
isSorted [_] = True
isSorted (x1 : x2 : xs)
        | x1 <= x2  = isSorted (x2 : xs)
        | otherwise = False

data SearchError = Unsorted deriving (Show)

showError :: SearchError -> String
showError Unsorted = "This list is unsorted."

binarySearch :: Ord a => [a] -> a -> Either SearchError Bool
binarySearch [] _             = Right False
binarySearch list target
        | not $ isSorted list = Left $ Unsorted
        | midVal == target    = Right True
        | midVal < target     = binarySearch (drop (mid + 1) list) target
        | midVal > target     = binarySearch (take mid list) target
            where
                mid    = div (length list) 2
                midVal = list !! mid

binarySearch' :: Ord a => [a] -> a -> Bool
binarySearch' [] _            = False
binarySearch' list target
        | midVal == target    = True
        | midVal < target     = binarySearch' (drop (mid + 1) list) target
        | midVal > target     = binarySearch' (take mid list) target
            where
                mid    = div (length list) 2
                midVal = list !! mid

binarySearch'' :: Ord a => [a] -> a -> Bool
binarySearch'' [] _           = False
binarySearch'' list target
        | not $ isSorted list = binarySearch'' (quickSort list) target
        | midVal == target    = True
        | midVal < target     = binarySearch'' (drop (mid + 1) list) target
        | midVal > target     = binarySearch'' (take mid list) target
            where
                mid    = div (length list) 2
                midVal = list !! mid
