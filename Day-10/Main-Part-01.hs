{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}

module Main (main) where
import qualified Data.Bifunctor
import qualified Data.Map as Map
import Data.Map (Map, (!))

main :: IO ()
main = do
    contents <- readFile "./app/data.txt"

    let items0 = zip [0,1..] (lines contents)
    let items1 = map (Data.Bifunctor.second (zip [0,1..])) items0
    let items2 = concatMap (filter (\x -> '.' /= snd x) . (\x -> map (\y -> (Point { x_p=fst y, y_p=fst x}, snd y)) $ snd x)) items1
    let items3 = map (\x -> GameSquare { c=snd x, pos=fst x, joined=uncurry neighbours x } ) items2

    let start = head $ filter (\x -> c x == 'S') items3
    let itemsNoStart = filter (\x -> c x /= 'S') items3
    let joinedToStart = filter (elem (pos start) . joined) itemsNoStart
    let startNew = GameSquare { c=c start, pos=pos start, joined=map pos joinedToStart }
    let points = startNew : itemsNoStart

    let pointsMap = Map.fromList $ map (\x0 -> (pos x0, x0)) points

    let visited2 = path pointsMap [(0, startNew)] (Map.empty :: Map Point Int)
    let maxCount = maximum $ snd visited2

    mapM_ print $ fst visited2

    print maxCount

path :: Map Point GameSquare -> [(Int, GameSquare)] -> Map Point Int -> ([(Int, GameSquare)], Map Point Int)
path pointsMap queue visited
    | null queue = ([], visited)
    | otherwise = do
        let stop = visitedCount /= (-1) && visitedCount <= count;
        let newVisited = if stop then visited else Map.insert point count visited
        let newNeighbours = if stop then [] else map (\x -> (count + 1, pointsMap ! x)) (joined gameSquare)
        let newQueue = tail queue ++ newNeighbours
        path pointsMap newQueue newVisited
    where
        dequeue = head queue;
        count = fst dequeue
        gameSquare = snd dequeue
        point = pos gameSquare
        visitedCount = if Map.member point visited then visited ! point else (-1)

data GameSquare = GameSquare { c :: Char, pos :: Point, joined :: [Point] } deriving (Show)
data Point = Point { x_p :: Int, y_p :: Int } deriving (Show, Ord)

instance Eq Point where
   x == y  = x_p x == x_p y && y_p x == y_p y

neighbours :: Point -> Char -> [Point]
neighbours p charac = map (\x0 -> Point { x_p=x_p p + x_p x0, y_p=y_p p + y_p x0}) $ vals charac

vals :: Char -> [Point]
vals '|' = [Point 0 (-1), Point 0 1]
vals '-' = [Point (-1) 0, Point 1 0]
vals 'L' = [Point 0 (-1), Point 1 0]
vals 'J' = [Point 0 (-1), Point (-1) 0]
vals '7' = [Point 0 1, Point (-1) 0]
vals 'F' = [Point 0 1, Point 1 0]
vals '.' = []
vals 'S' = []
vals _ = error "Unkown"