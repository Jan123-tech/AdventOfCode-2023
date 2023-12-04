{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}

module Main (main) where

import qualified Data.HashSet as HashSet
import qualified Data.Bifunctor
import Data.List.Split ( splitOn )

main :: IO ()
main = do
    contents <- readFile "./app/data.txt"
    let items = lines contents

    let items1 = map (splitOn " | " ) items
    let items2 = map (\x -> (head x, last x)) items1
    let items3 = map (Data.Bifunctor.first (splitOn ": ")) items2
    let items4 = map (\x -> let f = fst x in [last f, snd x]) items3
    let items5 = map (map ( filter (/= "") . splitOn " ")) items4
    let items6 = map (map (map (\x -> (read x :: Int)))) items5
    let items7 = zip [0..] items6
    let items8 = map (\x -> (map (\y -> (fst x, y)) . head $ snd x, map (\y -> (fst x, y)) . last $ snd x)) items7
    let items9 = concatMap fst items8 :: [(Int, Int)]

    let mp = HashSet.fromList items9
    let itemsToMatch = map snd items8
    let matchCounts0 = map (length . filter (`HashSet.member` mp)) itemsToMatch

    let matchCounts = length matchCounts0 : matchCounts0
    let cards = getcards 0 matchCounts
    let count = length cards

    print count

getcards :: Int -> [Int] -> [Int]
getcards n scores
    | score == 0 = []
    | otherwise = do
        let range = [n+1..n+score]
        let rangeCards = concatMap (`getcards` scores) range
        range ++ rangeCards
    where score = scores!!n