{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}

module Main (main) where

import Data.List (group, sortBy, sort)
import Data.Ord
import Data.Char (digitToInt)

main :: IO ()
main = do
    contents <- readFile "./app/data.txt"

    let hands0 = map ((\x -> (sortBy (comparing (Down . length)) . group . sort $ head x, head x, read $ last x :: Int)) . words) $ lines contents
    let hands1 = sortBy sort' hands0
    let hands2 = zip [1..] hands1  :: [(Int, ([[Char]], [Char], Int))]
    let totals = map (\x -> fst x * thd (snd x)) hands2
    let total = sum totals

    mapM_ print hands2
    mapM_ print totals

    print total

sort' :: ([[Char]], [Char], Int) -> ([[Char]], [Char], Int) -> Ordering
sort' x y
    | score_x > score_y = GT
    | score_x < score_y = LT
    | otherwise = sort2 (snd' x) (snd' y)
        where
            score_x = score $ fst' x
            score_y = score $ fst' y

sort2 :: [Char] -> [Char] -> Ordering
sort2 [] [] = GT
sort2 x y
    | score_x > score_y = GT
    | score_x < score_y = LT
    | otherwise = sort2 (tail x) (tail y)
        where
            score_x = scoreCard $ head x
            score_y = scoreCard $ head y

score :: [[Char]] -> Int
score x
    | len0 == 5 = 7
    | len0 == 4 = 6
    | len0 == 3 = if len1 == 2 then 5 else 4
    | len0 == 2 = if len1 == 2 then 3 else 2
    | otherwise = 1
        where
            len0 = length $ head x
            len1 = if len0 == 5 then 0 else length $ x!!1

scoreCard :: Char -> Int
scoreCard 'A' = 14
scoreCard 'K' = 13
scoreCard 'Q' = 12
scoreCard 'J' = 11
scoreCard 'T' = 10
scoreCard x = digitToInt x

fst' :: (a, b, c) -> a
fst' (a,_,_) = a

snd' :: (a, b, c) -> b
snd' (_,b,_) = b

thd :: (a, b, c) -> c
thd (_,_,c) = c