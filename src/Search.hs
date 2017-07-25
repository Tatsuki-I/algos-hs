module Search where

import Sort.QuickSort

sequentialSearch                      :: Eq a => [a] -> a -> Bool
sequentialSearch []     _             =  False
sequentialSearch (x:xs) t | x == t    =  True
                          | otherwise =  sequentialSearch xs t

{-
sequentialSearchMaybe :: Eq a => [a] -> a -> Maybe Int
sequentialSearchMaybe [] _ = Nothing
sequentialSearchMaybe list@(x : xs) target
        | x == target = Just (length xs)
        | otherwise   = sequentialSearchMaybe xs target
        -}

sequentialSearchMaybe      :: Eq a => [a] -> a -> Maybe Int
sequentialSearchMaybe xs t =  sSearchMaybe' xs t 0
  where
    sSearchMaybe'                        :: Eq a => [a] -> a -> Int -> Maybe Int
    sSearchMaybe' []     _ _             =  Nothing
    sSearchMaybe' (x:xs) t c | x == t    =  Just c
                             | otherwise =  sSearchMaybe' xs t (c + 1)

isSorted                        :: Ord a => [a] -> Bool
isSorted []                     =  True
isSorted [_]                    =  True
isSorted (x1:x2:xs) | x1 <= x2  =  isSorted (x2:xs)
                    | otherwise =  False

data SearchError = Unsorted deriving (Show)

showError          :: SearchError -> String
showError Unsorted =  "This list is unsorted."

binarySearch                          :: Ord a => [a] -> a -> Either SearchError Bool
binarySearch [] _                     =  Right  False
binarySearch xs t | not $ isSorted xs =  Left $ Unsorted
                  | midVal == t       =  Right  True
                  | midVal <  t       =  binarySearch (drop (mid + 1) xs) t
                  | midVal >  t       =  binarySearch (take mid xs)       t
                    where
                      mid    = div (length xs) 2
                      midVal = xs !! mid

binarySearch'                    :: Ord a => [a] -> a -> Bool
binarySearch' [] _               =  False
binarySearch' xs t | midVal == t =  True
                   | midVal <  t =  binarySearch' (drop (mid + 1) xs) t
                   | midVal >  t =  binarySearch' (take mid xs)       t
                     where
                       mid    = div (length xs) 2
                       midVal = xs !! mid

binarySearch''                          :: Ord a => [a] -> a -> Bool
binarySearch'' [] _                     =  False
binarySearch'' xs t | not $ isSorted xs =  binarySearch'' (quickSort xs)      t
                    | midVal == t       =  True
                    | midVal <  t       =  binarySearch'' (drop (mid + 1) xs) t
                    | midVal >  t       =  binarySearch'' (take mid xs)       t
                      where
                        mid    = div (length xs) 2
                        midVal = xs !! mid
