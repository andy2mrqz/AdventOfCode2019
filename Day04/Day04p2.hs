module Main where

import qualified Data.List as L

input = map show [278384..824795]

isOrdered :: Ord a => [a] -> Bool
isOrdered [x] = True
isOrdered (x:y:xs) = x <= y && isOrdered(y:xs)

criteria :: String -> Bool
criteria pw =
    length pw == 6 &&
    (any (\g -> (length g) == 2) $ L.group pw) &&
    (isOrdered pw)

main = print $ length $ filter criteria input
