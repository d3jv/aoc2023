import System.IO
import Data.List
import Data.List.Split

main = do handle <- openFile "input" ReadMode
          contents <- hGetContents handle
          let _lines = lines contents
          putStr "Task1: "
          (putStr . show . task1 . parse) _lines
          putStr "\nTask2: "
          (putStr . show . task2 . parse) _lines
          putStr "\n"
          return ()

--               id  win   have
data Card = Card Int [Int] [Int]

data CardScore = CardScore Int Int
                deriving (Show)

parse :: [String] -> [Card]
parse = map parseCard
    where
        parseCard :: String -> Card
        parseCard str = let card = splitOn ":" str
                            numbers = map ((map read) . words) $ splitOn "|" $ last card
                            index = read $ last $ words $ head card 
                        in Card index (head numbers) (last numbers)

getScore :: Card -> Int
getScore (Card _ win have) = let len = length $ intersect win have
                             in if len == 0 then 0 else 2 ^ (len - 1)

task1 :: [Card] -> Int
task1 = sum . (map getScore)

getNumberOfMatches :: Card -> Int
getNumberOfMatches (Card _ win have) = length $ intersect win have

incrementRangeBy :: Int -> Int -> Int -> [Int] -> [Int]
incrementRangeBy from count by xs = let start = take from xs
                                        middle = take count $ drop from xs
                                        end = drop (from + count) xs
                                    in start ++ map (+ by) middle ++ end

-- Unlike task1, this works only if the Card array is sorted by id.
-- That can be assured easily but I already got the star so...
task2 :: [Card] -> Int
task2 xs = let scores = map (\x@(Card id _ _) -> CardScore id $ getNumberOfMatches x) xs
           in task scores $ replicate (length scores) 1
    where
        task ((CardScore id score):ys) counts =
                let count = counts !! (id - 1)
                in count + task (ys) (incrementRangeBy id score count counts)
        task [] _ = 0

