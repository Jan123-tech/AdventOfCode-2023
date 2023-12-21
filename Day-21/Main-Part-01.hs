module Main (main) where
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Bifunctor
import Data.List (nub)

main :: IO ()
main = do
    contents <- readFile "./app/data.txt"

    let items0 = zip [0,1..] (lines contents)
    let items1 = map (Data.Bifunctor.second (zip [0,1..])) items0
    let items2 = concatMap ((\x -> map (\y -> (Point { x_p=fst y, y_p=fst x}, snd y)) $ snd x)) items1

    let x_points = map (\x -> x_p $ fst x) items2
    let y_points = map (\x -> y_p $ fst x) items2
    let min_x = (minimum $ x_points)
    let max_x = (maximum $ x_points)
    let min_y = (minimum $ y_points)
    let max_y = (maximum $ y_points)
    let x_all = [min_x,min_x+1..max_x]
    let y_all = [min_y,min_y+1..max_y]

    let start = head $ filter (\x -> snd x == 'S') items2
    let rocks_map = Map.fromList $ map (\x -> (fst x, x)) $ filter (\x -> '#' == snd x) items2
    let newPoints = steps 64 (min_x, min_y, max_x, max_y) rocks_map [fst start]
    let points_map = Map.fromList $ map (\x -> (x, True)) newPoints

    let rows = [ [let p = Point { x_p=x, y_p=y } in if Map.member p points_map then 'O' else if Map.member p rocks_map then '#' else if p == fst start then 'S' else '.' | x <- x_all] | y <- y_all]

    mapM_ print rows

    print $ length points_map

steps :: Int -> (Int, Int, Int, Int) -> Map Point (Point, Char) -> [Point] -> [Point]
steps 0 _ _ points = points
steps count (min_x, min_y, max_x, max_y) rocks_map points = do
    steps (count-1) (min_x, min_y, max_x, max_y) rocks_map (nub $ concatMap (\x -> getPoints (min_x, min_y, max_x, max_y) x rocks_map) points)

getPoints :: (Int, Int, Int, Int) -> Point -> Map Point (Point, Char) -> [Point]
getPoints (min_x, min_y, max_x, max_y) point rocks = do
        let newPoints0 = map (\(x, y) -> Point { x_p=x_p point + x, y_p=y_p point + y }) [(1,0),(-1,0),(0,-1),(0,1)]
        let newPoints1 = filter (\x -> x_p x >= min_x && x_p x <= max_x && y_p x >= min_y && y_p x <= max_y) newPoints0
        let newPoints2 = filter (\x -> Map.notMember x rocks) newPoints1
        newPoints2

data Point = Point { x_p :: Int, y_p :: Int } deriving (Show, Ord)

instance Eq Point where
   x == y  = x_p x == x_p y && y_p x == y_p y