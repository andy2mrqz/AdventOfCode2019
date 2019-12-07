module Main where

import Data.Char (digitToInt)

input = map show [278384..824795]

criteria :: String -> Bool
criteria pw =
    length pw == 6 &&
    (snd $ foldl (\(prev, adj) n -> (n, adj || (prev == n))) ('x', False) pw) && 
    (snd $ foldl (\(prev, incr) n -> (n, incr && (digitToInt prev <= digitToInt n))) ('0', True) pw)

main = print $ length $ filter criteria input
