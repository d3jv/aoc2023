import System.IO
import Data.Char
import Data.List.Extra hiding (splitOn, reverse)
import Data.Maybe

main = do handle <- openFile "input" ReadMode
          contents <- hGetContents handle
          putStr "Task1: "
          (putStr . show . task1) (lines contents)
          putStr "\nTask2: "
          (putStr . show . task2) (lines contents)
          putStr "\n"
          return ()

task1 :: [String] -> Int
task1 [] = 0
task1 (x:xs) = (getFirst x) * 10 + (getFirst (reverse x)) + task1 xs
    where
        getFirst (x:xs) = if isDigit x then digitToInt x else getFirst xs
        getFirst [] = 0

task2 :: [String] -> Int
task2 [] = 0
task2 (x:xs) = first * 10 + second + task2 xs
    where
        first = (head . (mapMaybe getFirstNumber) . tails) x
        second = (head . (mapMaybe getFirstNumber) . reverse . tails) x

getFirstNumber :: String -> Maybe Int
getFirstNumber [] = Nothing
getFirstNumber str@(x:_)
    | "one" `isPrefixOf` str = Just 1
    | "two" `isPrefixOf` str = Just 2
    | "three" `isPrefixOf` str = Just 3
    | "four" `isPrefixOf` str = Just 4
    | "five" `isPrefixOf` str = Just 5
    | "six" `isPrefixOf` str = Just 6
    | "seven" `isPrefixOf` str = Just 7
    | "eight" `isPrefixOf` str = Just 8
    | "nine" `isPrefixOf` str = Just 9
    | isDigit x = (Just . digitToInt) x
    | otherwise = Nothing

