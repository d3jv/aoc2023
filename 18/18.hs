import System.IO

main = do handle <- openFile "input" ReadMode
          contents <- hGetContents handle
          let _lines = lines contents
          putStr "Task1: "
          (putStr . show . task1) _lines
          putStr "\nTask2: "
          (putStr . show . task2) _lines
          putStr "\n"
          return ()

task1 :: [String] -> Int
task1 = undefined

task2 :: [String] -> Int
task2 = undefined

