module Main where

import Data.List (group, sort)

criteria :: String -> Bool
criteria pw =
    (any (>1) $ map length $ group pw) &&
    (pw == sort pw)

main = print $ length $ filter criteria $ map show [278384..824795]
