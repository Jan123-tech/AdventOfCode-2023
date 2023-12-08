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

    let count = find "AAA" (cycle instructions) nodeMap 0

    mapM_ print nodeMap

    print count

find :: [Char] -> [Char] -> Map.Map [Char] ([Char], [Char]) -> Int -> Int
find key instrs nodeMap count
    | nextNodeId == "ZZZ" = newCount
    | otherwise = find nextNodeId (tail instrs) nodeMap newCount
    where
        nextNodeId = let node = nodeMap ! key in if head instrs == 'L' then fst node else snd node
        newCount = count + 1