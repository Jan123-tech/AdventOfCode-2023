module Main (main) where

import Data.Map ((!))
import qualified Data.Map as Map
import Data.List.Split ( splitOn )
import Data.Bifunctor

main :: IO ()
main = do
    contents <- readFile "./app/data.txt"
    let items = lines contents
    let games0 = map (\s -> let split = splitOn ": " s in (head split, split!!1)) items
    let games1 = map (Data.Bifunctor.second (splitOn "; ")) games0
    let games2 = map (Data.Bifunctor.second (map (splitOn ", "))) games1
    let games3 = map (Data.Bifunctor.second (map (map ((\x -> (read $ head x :: Int, head $ x!!1)) . splitOn " ")))) games2
    let maxMap = Map.fromList [('r', 12),('g', 13),('b', 14)]
    let tooBig (a, b) = (maxMap ! b) < a
    let games4 = filter (\(_, b) -> not $ any (any tooBig) b) games3
    let games5 = map (first (last . splitOn " ")) games4
    let games6 = map (first (\x -> read x :: Int)) games5
    let total = sum $ map fst games6
    print total