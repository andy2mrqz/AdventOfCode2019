module Main where

import Data.Char (digitToInt)

input = map show [278384..824795]

isOrdered :: [Int] -> Bool
isOrdered [x] = True
isOrdered (x:y:xs) = x <= y && isOrdered(y:xs)

criteria :: String -> Bool
criteria pw =
    length pw == 6 &&
    (any (\(a,b) -> a == b) $ zip pw (tail pw)) &&
    (isOrdered $ map digitToInt pw)

main = print $ length $ filter criteria input
