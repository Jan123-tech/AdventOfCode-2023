module Main (main) where

import Data.List.Split ( splitOn )
import Data.List (groupBy, sortBy)
import Data.Function (on)

main :: IO ()
main = do
    contents <- readFile "./app/data.txt"
    let items = lines contents
    let games0 = map ((!! 1) . splitOn ": ") items
    let games1 = map (splitOn "; ") games0
    let games2 = map (map (splitOn ", ")) games1
    let games3 = map (concatMap (map ((\x -> (read $ head x :: Int, head $ x!!1)) . splitOn " "))) games2
    let games4 = map (groupBy (\a b -> snd a == snd b) . sortBy (compare `on` snd)) games3
    let getMax x = maximum $ map fst x
    let games5 = map (\x -> [getMax $ head x, getMax $ x!!1, getMax $ x!!2]) games4
    let games6 = map product games5
    let total = sum games6
    print total