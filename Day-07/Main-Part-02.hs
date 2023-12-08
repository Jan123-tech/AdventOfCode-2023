{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}

module Main (main) where

import Data.List (group, sortBy, sort, maximumBy)
import Data.Ord
import Data.Char (digitToInt)
import Data.Function (on)
import qualified Control.Arrow as Data.Bifunctor

main :: IO ()
main = do
    contents <- readFile "./app/data.txt"

    let hands0 = map ((\x -> (head x, read $ last x :: Int)) . words) $ lines contents

    -- Debug
    --let items0 = map fst hands0
    --let items1 = map (\x -> (x, getCombos x)) items0

    --mapM_ print items1

    --let items2 = map (Data.Bifunctor.second (map (\x -> (x, score x)))) items1
    --let items3 = map (Data.Bifunctor.second (maximumBy (compare `on` snd))) items2

    --mapM_ print items3

    let hands1 = sortBy sort' hands0
    let hands2 = zip [1..] hands1 :: [(Int, ([Char], Int))]
    let totals = map (\x -> fst x * snd (snd x)) hands2
    let total = sum totals

    --mapM_ print hands2
    --mapM_ print totals

    print total

sort' :: ([Char], Int) -> ([Char], Int) -> Ordering
sort' x y
    | score_x > score_y = GT
    | score_x < score_y = LT
    | otherwise = sort2 (fst x) (fst y)
        where
            score_x = maximum $ map score $ getCombos $ fst x
            score_y = maximum $ map score $ getCombos $ fst y

sort2 :: [Char] -> [Char] -> Ordering
sort2 [] [] = GT
sort2 x y
    | score_x > score_y = GT
    | score_x < score_y = LT
    | otherwise = sort2 (tail x) (tail y)
        where
            score_x = scoreCard $ head x
            score_y = scoreCard $ head y

getCombos :: [Char] -> [[Char]]
getCombos x = do
    let nonJokers = ['2', '3', '4', '5', '6', '7', '8', '9', 'J', 'T', 'Q', 'K', 'A']
    let numJokers = length $ filter (== 'J') x

    let combos = case numJokers of
            0 -> [[]]
            1 -> [ [j0] | j0 <- nonJokers] 
            2 -> [ [j0, j1] | j0 <- nonJokers, j1 <- nonJokers]
            3 -> [ [j0, j1, j2] | j0 <- nonJokers, j1 <- nonJokers, j2 <- nonJokers]
            4 -> [ [j0, j1, j2, j3] | j0 <- nonJokers, j1 <- nonJokers, j2 <- nonJokers, j3 <- nonJokers]
            5 -> [ [j0, j1, j2, j3, j4] | j0 <- nonJokers, j1 <- nonJokers, j2 <- nonJokers, j3 <- nonJokers, j4 <- nonJokers]
            _ -> error "Too many jokers"

    let combos0 = map (fillIn x) combos

    combos0

fillIn :: [Char] -> [Char] -> [Char]
fillIn [] _ = []
fillIn cards combos =
        let c = head cards;
                isJoker = c == 'J';
                cNew = if isJoker then head combos else c;
                remaining = if isJoker then tail combos else combos in
        cNew : fillIn (tail cards) remaining

score :: [Char] -> Int
score x
    | len0 == 5 = 7
    | len0 == 4 = 6
    | len0 == 3 = if len1 == 2 then 5 else 4
    | len0 == 2 = if len1 == 2 then 3 else 2
    | otherwise = 1
        where
            arr = sortBy (comparing (Down . length)) . group $ sort x
            len0 = length $ head arr
            len1 = if len0 == 5 then 0 else length $ arr!!1

scoreCard :: Char -> Int
scoreCard 'A' = 14
scoreCard 'K' = 13
scoreCard 'Q' = 12
scoreCard 'J' = 1
scoreCard 'T' = 10
scoreCard x = digitToInt x