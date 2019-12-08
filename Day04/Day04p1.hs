module Main where

import Data.Char (digitToInt)

input = map show [278384..824795]

isOrdered :: [Int] -> Bool
isOrdered [x] = True
isOrdered (x:y:xs) = x <= y && isOrdered(y:xs)

eqlTpl :: (Char, Char) -> Bool
eqlTpl (a,b) = a == b

stagger :: [a] -> [(a, a)]
stagger str = zip str (tail str)

criteria :: String -> Bool
criteria pw =
    length pw == 6 &&
    (any eqlTpl $ stagger pw) &&
    (isOrdered $ map digitToInt pw)

main = print $ length $ filter criteria input
