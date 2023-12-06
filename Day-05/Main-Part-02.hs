{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}

module Main (main) where

import Data.List.Split ( splitOn, chunksOf )

main :: IO ()
main = do
    contents <- readFile "./app/data.txt"

    let sections0 = splitOn "\n\n" contents

    let seeds0 = head sections0
    let seeds1 = words seeds0
    let seeds2 = tail seeds1
    let seeds3 = map (\x -> read x :: Int) seeds2
    let seeds4 = chunksOf 2 seeds3

    let sections1 = map (tail . splitOn "\n") $ tail sections0
    let sections2 = map (map words) sections1
    let sections = map (map (map (\z -> read z :: Int))) sections2

    let mins = map (`doSeeds` sections) seeds4

    mapM_ print mins

    let mini = minimum mins

    print mini


doSeeds :: [Int] -> [[[Int]]] -> Int
doSeeds seeds sections = do
    let seeds01 = (\x -> let start = head x; end = start + (last x - 1); in [start..end]) seeds
    let values = map (\x -> foldl getValue x sections) seeds01
    let minLoc = minimum values
    minLoc

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