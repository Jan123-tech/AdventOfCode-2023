{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}

module Main (main) where
import Data.List.Split (splitOn)
import qualified Data.Map as Map

main :: IO ()
main = do
    contents <- readFile "./app/data.txt"

    let items0 = lines contents
    let items1 = map (splitOn " ") items0
    let items2 = map (\x -> (getVector $ head x, [1,2..read $ x!!1 :: Int], last x)) items1
    
    let points = foldl (\acc x -> getPoints acc (fst' x) (snd' x)) [Point { x_p=0, y_p=0 }] items2
    let points_map = Map.fromList $ map (\x -> (x, True)) points

    let points_list = Map.toList points_map
    let x_points = map (\x -> x_p $ fst x) points_list
    let y_points = map (\x -> y_p $ fst x) points_list
    let min_x = (minimum $ x_points) - 1
    let max_x = (maximum $ x_points) + 1
    let min_y = (minimum $ y_points) - 1
    let max_y = (maximum $ y_points) + 1
    let x_all = [min_x,min_x+1..max_x]
    let y_all = [min_y,min_y+1..max_y]

    let outsidePoints = path (min_x, min_y, max_x, max_y) [Point { x_p=min_x, y_p=min_y }] Map.empty points_map

    let rows = [ [let p = Point { x_p=x, y_p=y } in if Map.member p points_map then '#' else if Map.member p outsidePoints then '.' else '+' | x <- x_all] | y <- y_all]

    mapM_ print rows

    let countBorder = length points_map
    let countInside = length $ concatMap (\x -> filter (\y -> y == '+') x) rows

    let total = countBorder + countInside

    print total

path :: (Int, Int, Int, Int) -> [Point] -> Map Point Bool -> Map Point Bool -> Map Point Bool
path (min_x, min_y, max_x, max_y) queue visited border
    | null queue = visited
    | otherwise = do
        let point = head queue;
        let newPoints0 = map (\(x, y) -> Point { x_p=x_p point + x, y_p=y_p point + y }) [(1,0),(-1,0),(0,-1),(0,1)]
        let newPoints1 = filter (\x -> x_p x >= min_x && x_p x <= max_x && y_p x >= min_y && y_p x <= max_y) newPoints0
        let newPoints2 = filter (\x -> Map.notMember x visited) newPoints1
        let newPoints3 = filter (\x -> Map.notMember x border) newPoints2
        let newVisited = foldl (\acc x -> Map.insert x True acc) visited newPoints3
        let newQueue = tail queue ++ newPoints3
        path (min_x, min_y, max_x, max_y) newQueue newVisited border

getPoints :: [Point] -> Point -> [Int] -> [Point]
getPoints existingPoints _ [] = existingPoints
getPoints existingPoints direction count = do
        let start = head existingPoints
        let newPoints = Point { x_p=x_p start + x_p direction, y_p=y_p start + y_p direction } : existingPoints
        getPoints newPoints direction (tail count)
 
data Point = Point { x_p :: Int, y_p :: Int } deriving (Show, Ord)

instance Eq Point where
   x == y  = x_p x == x_p y && y_p x == y_p y

getVector :: [Char] -> Point
getVector "R" = Point { x_p=1, y_p=0 }
getVector "L" = Point { x_p=(-1), y_p=0 }
getVector "U" = Point { x_p=0, y_p=(-1) }
getVector "D" = Point { x_p=0, y_p=1 }
getVector _ = error("error")

fst' :: (a, b, c) -> a
fst' (a,_,_) = a

snd' :: (a, b, c) -> b
snd' (_,b,_) = b