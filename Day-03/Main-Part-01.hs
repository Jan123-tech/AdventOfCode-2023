{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}

module Main (main) where

import Data.List (groupBy, sort, group)
import qualified Data.Bifunctor
import Data.Map ((!))
import qualified Data.Map as Map

main :: IO ()
main = do
    contents <- readFile "./app/data.txt"
    let items = lines contents

    let itemsWithIndex = zip [0..] items :: [(Int, String)]

    let numberPoints = concatMap getNumbers itemsWithIndex
    let pointsMap = Map.fromList numberPoints

    let symbolPoints = concatMap getSymbols itemsWithIndex

    let newPointVectors = [(-1,0),(-1,1),(0,1),(1,1),(1,0),(1,-1),(0,-1),(-1,-1)] :: [(Int, Int)]

    let allPointsToCheck0 = map (\((y, x), c) -> (map (\(yv, xv) -> (yv + y, xv + x)) newPointVectors, ((y, x), c))) symbolPoints
    let allPointsToCheck1 = map head . group . sort $ concatMap fst allPointsToCheck0

    let matches = map (pointsMap !) $ filter (`Map.member` pointsMap) allPointsToCheck1
    let uniqueMaches = map (snd . head) . group $ sort matches
    let total = sum uniqueMaches

    mapM_ print uniqueMaches

    print total

isDigit :: Char -> Bool
isDigit x = x `elem` ['0','1','2','3','4','5','6','7','8','9']

getNumbers :: (Int, String) -> [((Int, Int), ((Int, Int), Int))]
getNumbers (rowIndex, row) = do
    let addindex1 x = zip [0..] x :: [(Int, Char)]
    let addindex2 x = zip [0..] x :: [(Int, [(Int, Char)])]
    let numbers0 = filter (isDigit . snd . head) $ groupBy (\(_, a) (_, b) -> isDigit a && isDigit b) $ addindex1 row
    let numbers1 = addindex2 numbers0
    let numbers2 = map (Data.Bifunctor.second unzip) numbers1
    let numbers3 = map (\(i, x) -> (fst x, (i, read $ snd x :: Int ))) numbers2
    let numbers4 = map (\(a, b) -> (map (rowIndex :: Int,) a, ((rowIndex, fst b), snd b))) numbers3
    let numbers5 = map (\(a, b) -> map (\x -> (x, b)) a) numbers4
    let numbers6 = concat numbers5
    numbers6

getSymbols :: (Int, String) -> [((Int, Int), Char)]
getSymbols (rowIndex, row) = do
    let line1WithIndex = zip [0..] row :: [(Int, Char)]
    let symbols = map (\(i, x) -> ((rowIndex, i), x)) $ filter (\(_, x) -> not $ isDigit x || x == '.') line1WithIndex
    symbols