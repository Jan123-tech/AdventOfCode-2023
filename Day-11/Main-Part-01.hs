{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}

module Main (main) where
import qualified Data.Bifunctor

main :: IO ()
main = do
    contents <- readFile "./app/data.txt"

    let items = lines contents
    let items0 = concatMap (\x -> if all (== '.') x then [x, x] else [x] ) items
    let items1 = map (\x -> map (!! x) $ reverse items0) [0,1..length (head items0)-1]
    let items2 = concatMap (\x -> if all (== '.') x then [x, x] else [x] ) items1
    let items3 = map (\x -> map (!! x) items2) $ reverse [0,1..length (head items2)-1]

    let items4 = zipWith (curry (Data.Bifunctor.second (zip [0,1..]))) [0,1..] items3
    let items5 = concatMap (filter (\x -> thd x == '#' ) . (\x -> map (\y -> (fst y, fst x, snd y)) $ snd x)) items4
    let items6 = zip [1::Int,2..] items5
    let items7 = map (\x -> (fst' $ snd x, snd' $ snd x, show $ fst x)) items6

    let pointPairs = filter (uncurry (/=)) $ [ (x, y) | x <- items7, y <- items7]
    let distances = map (\x -> let a = fst x; b = snd x in abs (fst' a - fst' b) + abs (snd' a - snd' b)) pointPairs
    let total = sum distances

    mapM_ print distances

    print $ total / 2

fst' :: (a, b, c) -> a
fst' (a,_,_) = a

snd' :: (a, b, c) -> b
snd' (_,b,_) = b

thd :: (a, b, c) -> c
thd (_,_,c) = c