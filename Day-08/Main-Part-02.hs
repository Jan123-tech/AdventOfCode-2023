{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}

module Main (main) where

import Data.List.Split ( splitOn )
import qualified Data.Map as Map
import Data.Map ((!))

main :: IO ()
main = do
    contents <- readFile "./app/data.txt"

    let items = lines contents

    let instructions = head items

    let nodes0 = drop 2 items
    let nodes1 = map (\s -> let split = splitOn " = " s in (head split, last split)) nodes0
    let nodes2 = map (\x -> let split = splitOn ", " $ snd x in (fst x, (drop 1 $ head split, let right = last split in take (length right - 1) right))) nodes1

    let nodeMap = Map.fromList nodes2
    let newKeys keys instr = map (\x -> let node = nodeMap ! x in if instr == 'L' then fst node else snd node) keys

    let period1 = getPeriod 0 nodes2 newKeys instructions
    let period2 = getPeriod 3 nodes2 newKeys instructions

    print period1
    print period2
    
    let s0 = [period2, 2*period2..]
    let s1 = head $ filter (\x -> x `mod` period1 == 0) s0

    print s1

getPeriod :: Int -> [([Char], ([Char], [Char]))] -> ([[Char]] -> Char -> [[Char]]) -> String -> Int
getPeriod d nodes2 newKeys instructions = do
    let startNodes = take 3 . drop d . map fst $ filter (\x -> drop 2 (fst x) == "A") nodes2
    let moreNodes = scanl (\acc instr -> (fst acc + 1, newKeys (snd acc) instr)) (0 :: Int, startNodes) (cycle instructions)
    fst $ head $ take 1 $ filter (all (\x -> drop 2 x == "Z") . snd) moreNodes