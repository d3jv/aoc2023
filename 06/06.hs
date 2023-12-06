import System.IO
import Data.List.Split

main = do handle <- openFile "input" ReadMode
          contents <- hGetContents handle
          let _lines = lines contents
          putStr "Task1: "
          (putStr . show . task1 . parse) _lines
          putStr "\nTask2: "
          (putStr . show . task2 . parse2) _lines
          putStr "\n"
          return ()

parse :: [String] -> [(Int, Int)]
parse xs = let [times, distances] = map ((map read) . words . last . (splitOn ":")) xs
           in zip times distances

task1 :: [(Int, Int)] -> Int
task1 = (foldr (*) 1) . map (\(t, d) -> countRecords 0 t d)
        
countRecords hold time dist = 
        if hold == time then 0
        else if not $ canBeat hold time dist
             then countRecords (hold + 1) time dist
             else time - (2 * hold - 1)
    where
        canBeat hold time dist = (time - hold) * hold > dist

parse2 :: [String] -> (Int, Int)
parse2 xs = let [t, d] = map (readInt . (filter (/= ' ')) . last . (splitOn ":")) xs
            in (t, d)
    where
        readInt x = read x :: Int

task2 :: (Int, Int) -> Int
task2 (t, d) = countRecords 0 t d

