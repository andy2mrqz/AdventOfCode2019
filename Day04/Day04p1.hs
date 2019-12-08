module Main where

import Data.Char (digitToInt)
import qualified Data.List as L

input = map show [278384..824795]

isOrdered :: [Int] -> Bool
isOrdered [x] = True
isOrdered (x:y:xs) = x <= y && isOrdered(y:xs)

criteria :: String -> Bool
criteria pw =
    length pw == 6 &&
    (any (\g -> (length g) > 1) $ L.group pw) &&
    (isOrdered $ map digitToInt pw)

main = print $ length $ filter criteria input
