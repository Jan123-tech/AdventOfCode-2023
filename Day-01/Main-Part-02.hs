module Main (main) where

import Lib()
import Text.Regex.Posix
import Data.Map ((!))
import qualified Data.Map as Map

main :: IO ()
main = do
    contents <- readFile "./app/data.txt"
    let items = lines contents
    let reg1 = "(one|two|three|four|five|six|seven|eight|nine|[0-9])"
    let reg2 = "(enin|thgie|neves|xis|evif|ruof|eerht|owt|eno|[0-9])"
    let numPairs = map (\s -> (s =~ reg1, reverse $ let sr = reverse s in sr =~ reg2)) items
    let numMap = Map.fromList [("one", "1"),("two", "2"),("three", "3"),("four", "4"),("five", "5") ,("six", "6"),("seven", "7"),("eight", "8"),("nine", "9")]
    let mapNum = (\x -> if Map.member x numMap then numMap ! x else x)
    let digitPairs = map (\(a, b) -> (mapNum a, mapNum b)) numPairs
    let numbers = map (\(a, b) -> a ++ b) digitPairs
    let ints = map (\x -> read x :: Int) numbers
    let total = sum ints
    putStrLn $ show total