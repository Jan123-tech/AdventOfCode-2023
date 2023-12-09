{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}

module Main (main) where

main :: IO ()
main = do
    contents <- readFile "./app/data.txt"

    let items0 = map (map (\y -> read y :: Int) . words) $ lines contents
    let items2 = map (\x -> diffs2 [x]) items0

    mapM_ print items2

    let items3 = map (map last) items2

    mapM_ print items3
    
    let items4 = map vals items3

    mapM_ print items4

    let total = sum $ map head items4

    print total

vals :: [Int] -> [Int]
vals [_] = [0]
vals x = let x0 = vals (tail x) in head x + head x0 : x0

diffs2 :: [[Int]] -> [[Int]]
diffs2 x
    | all (== 0) $ last x = x
    | otherwise = diffs2 $ x ++ [diffs (last x)]

diffs :: [Int] -> [Int]
diffs [_] = []
diffs x = (x!!1 - head x) : diffs (tail x)

