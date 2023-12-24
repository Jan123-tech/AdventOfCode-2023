module Main (main) where
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Bifunctor
import Data.List (nub)
import Data.List.Split (splitOn)
import Data.Map ((!), empty)

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

    let slope_map = Map.fromList $ map (\x -> (fst x, x)) $ filter (\x -> '>' == snd x ||  '<' == snd x || '^' == snd x ||  'v' == snd x) items2
    let rocks_map = Map.fromList $ map (\x -> (fst x, x)) $ filter (\x -> '#' == snd x) items2
    let points_map = Map.fromList $ map (\x -> (fst x, x)) items2

    let prevPoint = Point { x_p=1, y_p=(-1) }
    let startPoint = Point { x_p=1, y_p=0 }
    let endPoint = Point { x_p=max_x-1, y_p=max_y }
    let paths = getPoints endPoint startPoint prevPoint points_map Map.empty []
    let pathPoints = Map.fromList $ map (\x -> (x, True)) $ concatMap (\x -> x) paths

    let rows = [ [let p = Point { x_p=x, y_p=y } in if Map.member p pathPoints then 'O' else if Map.member p rocks_map then '#' else if Map.member p slope_map then snd $ slope_map ! p else '.' | x <- x_all] | y <- y_all]
    mapM_ print rows

    print 'v'

getPoints :: Point -> Point -> Point ->  Map Point (Point, Char) -> Map Point Bool -> [Point] -> [Point]
getPoints endPoint point previousPoint squaresMap visited paths
    | point == endPoint || null newPoints = paths
    | otherwise = do
        let newVisited = foldl (\acc x -> Map.insert x True acc) visited newPoints
        foldl (\acc x -> getPoints endPoint x point squaresMap newVisited acc) newPoints visited
            where
                newPointsSource = map (\(x, y) -> Point { x_p=x_p point + x, y_p=y_p point + y }) [(1,0),(-1,0),(0,-1),(0,1)];
                newPoints = filter (\x -> Map.notMember x visited && canEnter (snd $ squaresMap ! x) previousPoint x) newPointsSource;

data Point = Point { x_p :: Int, y_p :: Int } deriving (Show, Ord)

instance Eq Point where
   x == y  = x_p x == x_p y && y_p x == y_p y

canEnter :: Char -> Point -> Point -> Bool
canEnter c pBefore p
    | c == '.' = True
    | c == '#' = False
    | c == '>' && diff == Point { x_p=1, y_p=0 } = True
    | c == '<' && diff == Point { x_p=(-1), y_p=0 } = True
    | c == '^' && diff == Point { x_p=0, y_p=(-1) } = True
    | c == 'v' && diff == Point { x_p=0, y_p=1 } = True
    | otherwise = False
        where diff = Point { x_p=(x_p p) - (x_p pBefore), y_p=(y_p p) - (y_p pBefore) };