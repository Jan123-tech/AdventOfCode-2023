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

    let s0 = [77241539 :: Integer, 154483078..]
    let s1 = head $ filter (\x -> x `mod` 49803847 == 0) s0

    print s1

    let instructions = head items

    let nodes0 = drop 2 items
    let nodes1 = map (\s -> let split = splitOn " = " s in (head split, last split)) nodes0
    let nodes2 = map (\x -> let split = splitOn ", " $ snd x in (fst x, (drop 1 $ head split, let right = last split in take (length right - 1) right))) nodes1

    let nodeMap = Map.fromList nodes2

    let startNodes = take 3 . drop 3 . map fst $ filter (\x -> drop 2 (fst x) == "A") nodes2

    let newKeys keys instr = map (\x -> let node = nodeMap ! x in if instr == 'L' then fst node else snd node) keys

    let moreNodes = scanl (\acc instr -> (fst acc + 1, newKeys (snd acc) instr)) (0 :: Int, startNodes) (cycle instructions)

    let count = take 5 $ filter (all (\x -> drop 2 x == "Z") . snd) moreNodes

    mapM_ print count

    print 'v'