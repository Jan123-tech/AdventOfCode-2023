module Main (main) where
import Data.List (nub)
import Data.List.Split (splitOn)

main :: IO ()
main = do
    contents <- readFile "./app/data.txt"

    let items0 = map (nub . splitOn " ") $ lines contents
    let items1 = map (\x -> filter (\y -> fst y > 1) $ zip [0 :: Int,1..] x) items0
    let items2 = [ (snd x, snd $ head $ filter (\y -> fst x == fst y) $ last items1) | x <- head items1]
    let items3 = map (\x -> (read $ fst x :: Int, read $ snd x :: Int)) items2

    let nums = map (\x -> getMax (fst x) (snd x)) items3

    print $ product nums

getMax :: Int -> Int -> Int
getMax maxSeconds record = do
    let times = map (\x -> let remaining = maxSeconds - x in [x, remaining, remaining * x]) [0,1..maxSeconds]
    let greaterThan = filter (\x -> last x > record) times
    length greaterThan