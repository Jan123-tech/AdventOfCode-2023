{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}

module Main (main) where
import Data.List.Split (splitOn)
import Data.Char (ord)
import qualified Data.Map as Map
import Data.Map (Map, (!), empty)

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
    let min_x = minimum $ x_points
    let max_x = maximum $ x_points
    let min_y = minimum $ y_points
    let max_y = maximum $ y_points
    let x_all = [min_x,min_x+1..max_x]
    let y_all = [min_y,min_y+1..max_y]

    let rows = zip [min_y,min_y+1..] [ [let p = Point { x_p=x, y_p=y } in (x_p p, if Map.member p points_map then '#' else '.') | x <- x_all] | y <- y_all]

    let insidePoints = foldl (\acc x -> getInsidePoints (fst x) (snd x) False False acc) [] rows

    let insidePointsMap = Map.fromList $ map (\x -> (x, True)) insidePoints

    let rows2 = [ [let p = Point { x_p=x, y_p=y } in if Map.member p points_map then '#' else if Map.member p insidePointsMap then '+' else '.' | x <- x_all] | y <- y_all]

    mapM_ print rows2

    print 'v'

getInsidePoints :: Int -> [(Int, Char)] -> Bool -> Bool -> [Point] -> [Point]
getInsidePoints _ [] _ _ points = points
getInsidePoints rowIndex row doCount wasHash currentPoints = do
        let newDoCount = if isPeriod && wasHash then not doCount else doCount
        let newCurrentPoints = if isPeriod && newDoCount then Point { x_p=index, y_p=rowIndex } : currentPoints else currentPoints
        getInsidePoints rowIndex (tail row) newDoCount isHash newCurrentPoints
            where currentSqaure = head row;
                    index = fst currentSqaure;
                    charac = snd currentSqaure;
                    isPeriod = charac == '.';
                    isHash = charac == '#'

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

thd :: (a, b, c) -> c
thd (_,_,c) = c