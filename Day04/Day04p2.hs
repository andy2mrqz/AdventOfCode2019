module Main where

import qualified Data.List as L

input = map show [278384..824795]

isOrdered :: Ord a => [a] -> Bool
isOrdered [x] = True
isOrdered (x:y:xs) = x <= y && isOrdered(y:xs)

criteria :: String -> Bool
criteria pw =
    (any (==2) . map length . L.group $ pw) &&
    (isOrdered pw)

main = print $ length $ filter criteria input
