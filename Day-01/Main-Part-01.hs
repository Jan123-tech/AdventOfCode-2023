module Main (main) where

import Text.Regex.Posix

main :: IO ()
main = do
    contents <- readFile "./app/data.txt"
    let items = lines contents
    let numPairs = map (\s -> let reg = "[0-9]" in s =~ reg ++ (reverse s) =~ reg) items
    let total = sum $ map (\x -> read x :: Int) numPairs
    print total