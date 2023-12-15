{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}

module Main (main) where
import Data.List.Split (splitOn)
import Data.Char (ord)

main :: IO ()
main = do
    contents <- readFile "./app/data.txt"

    let sections0 = splitOn "," contents

    let hashes = map (foldl (\acc x -> let s = acc + ord x in (*) 17 s `rem` 256) 0) sections0

    mapM_ print hashes

    print $ sum hashes