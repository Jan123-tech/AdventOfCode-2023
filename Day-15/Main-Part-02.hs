{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}

module Main (main) where
import Data.List.Split (splitOn)
import Data.Char (ord)
import qualified Data.Map as Map
import Data.Map (Map, (!), toList)
import qualified Data.Bifunctor

main :: IO ()
main = do
    contents <- readFile "./app/data.txt"

    let sections0 = splitOn "," contents
    let items = map (splitOn "=") sections0
    let items1 = map (\x -> if length x == 1 then let y = splitOn "-" $ head x in [head y, "-1"] else [head x, x!!1]) items
    let items2 = map (\x -> (head x, foldl (\acc x0 -> let s = acc + ord x0 in (*) 17 s `rem` 256) 0 $ head x, read $ x!!1 :: Int)) items1
    let items3 = load items2 (Map.empty :: Map Int [([Char], Int)])
    let items4 = toList items3
    let items5 = map (\x -> (fst x + 1, zip [1::Int,2..] $ snd x)) items4
    let items6 = map (Data.Bifunctor.second (map (Data.Bifunctor.second snd))) items5
    let items7 = map (Data.Bifunctor.second (map (uncurry (*)))) items6
    let items8 = map (\x -> map (\y -> fst x * y) $ snd x) items7
    let total = sum $ map sum items8

    mapM_ print items8

    print total

load :: [([Char], Int, Int)] -> Map Int [([Char], Int)] -> Map Int [([Char], Int)]
load [] m = m
load x m = do
    let item = head x
    let box = snd' item
    let arr = if Map.member box m then m ! box else []
    let arrNew = update (fst' item, thd item) arr
    let mNew = Map.insert box arrNew m
    load (tail x) mNew

update :: ([Char], Int) -> [([Char], Int)] -> [([Char], Int)]
update (label, -1) arr = filter (\x -> fst x /= label) arr
update (label, lens) arr = do
    let exists = any (\x0 -> fst x0 == label) arr
    let newItem = (label, lens)
    let arrNew = if exists then map (\x1 -> if fst x1 == label then newItem else x1) arr else arr ++ [newItem]
    arrNew

fst' :: (a, b, c) -> a
fst' (a,_,_) = a

snd' :: (a, b, c) -> b
snd' (_,b,_) = b

thd :: (a, b, c) -> c
thd (_,_,c) = c