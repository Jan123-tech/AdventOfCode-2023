{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}

module Main (main) where

import Data.List.Split ( splitOn )

main :: IO ()
main = do
    contents <- readFile "./app/data.txt"

    let sections0 = splitOn "\n\n" contents

    let seeds0 = head sections0
    let seeds1 = words seeds0
    let seeds2 = tail seeds1
    let seeds = map (\x -> read x :: Int) seeds2

    let sections1 = map (tail . splitOn "\n") $ tail sections0
    let sections2 = map (map words) sections1
    let sections = map (map (map (\z -> read z :: Int))) sections2

    let values = map (\x -> foldl getValue x sections) seeds
    let minLoc = minimum values

    print minLoc

getValue :: Int -> [[Int]] -> Int
getValue x [] = x
getValue x items = do
    let trial = head items
    let startRange = trial!!1
    let endRange = startRange + trial!!2
    if x >= startRange && x < endRange
    then do
        let offset = head trial - trial!!1
        x + offset
    else
        getValue x $ tail items