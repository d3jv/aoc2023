import System.IO
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

data Cubes = Red Int
           | Green Int
           | Blue Int

data Handful = RGB Int Int Int

data Game = Game Int [Handful]

parseCubes :: String -> Cubes
parseCubes str = foo $ (splitOn " ") str
    where
        foo :: [String] -> Cubes
        foo [x, "red"] = Red (read x)
        foo [x, "green"] = Green (read x)
        foo [x, "blue"] = Blue (read x)
        foo _ = error "Invalid arguments"

parseHandful :: String -> Handful
parseHandful = (makeHandful (RGB 0 0 0)). (map parseCubes) . (splitOn ", ")
    where
        makeHandful :: Handful -> [Cubes] -> Handful
        makeHandful hf@(RGB r g b) ((Red x):xs) = makeHandful (RGB (r+x) g b) xs
        makeHandful hf@(RGB r g b) ((Green x):xs) = makeHandful (RGB r (g+x) b) xs
        makeHandful hf@(RGB r g b) ((Blue x):xs) = makeHandful (RGB r g (b+x)) xs
        makeHandful hf [] = hf

parseArray :: String -> [Handful]
parseArray = (map parseHandful) . (splitOn "; ")

parseGame :: [String] -> Game
parseGame [game, x] = Game (getGameIndex $ splitOn " " game) (parseArray x)
    where
        getGameIndex ["Game", x] = read x
        getGameIndex _ = error "Invalid arguments"

parse :: [String] -> [Game]
parse = map (parseGame . (splitOn ": "))

getGameId :: Game -> Int
getGameId (Game i _) = i

getReds :: Handful -> Int
getReds (RGB r _ _) = r

getGreens :: Handful -> Int
getGreens (RGB _ g _) = g

getBlues :: Handful -> Int
getBlues (RGB _ _ b) = b

isPossible :: Handful -> Handful -> Bool
isPossible maxValues@(RGB r g b) (RGB x y z) = x <= r && y <= g && z <= b

task1 :: [Game] -> Int
task1 = (sum .(map getGameId) . (filter (\(Game _ xs) -> all (isPossible (RGB 12 13 14)) xs)))

getMinimum :: [Handful] -> Handful
getMinimum = foldr foo (RGB 0 0 0)
    where
        foo (RGB r g b) (RGB x y z) = RGB (if x > r then x else r)
                                          (if y > g then y else g)
                                          (if z > b then z else b)

getPower :: Handful -> Int
getPower (RGB r g b) = r * g * b

task2 :: [Game] -> Int
task2 = sum . (map (\(Game _ xs) -> getPower $ getMinimum xs))

