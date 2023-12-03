type Play = [Int]
data Game = Game { gameId :: Int, plays :: [Play] } deriving (Show)

splitOn :: Char -> String -> [String]
splitOn _ [] = []
splitOn delim str = fs : splitOn delim ls
    where (fs, ls) = break (== delim) (dropWhile (== delim) str)

basePlay :: [Int]
basePlay = [0,0,0]

colorToIndex :: String -> Int
colorToIndex "red" = 0
colorToIndex "green" = 1
colorToIndex "blue" = 2

toInt :: String -> Int
toInt s = read s :: Int

updatePlay :: Int -> Int -> Play -> Play
updatePlay count index play = [if i == index then play !! i + count else play !! i | i <- [0..length play - 1]]

parsePlayItem :: String -> String -> Play -> Play
parsePlayItem count color = updatePlay (toInt count) (colorToIndex color) 

-- [" 3 blue"," 4 red"]
parsePlay :: [String] -> Play
parsePlay xs = parsePlay' xs basePlay
    where
        parsePlay' [] play = play
        parsePlay' (x:xs) play = parsePlay' xs (parsePlayItem (head . words $ x) (last . words $ x) play)

-- [" 3 blue, 4 red", " 1 red, 2 green", " 6 blue, 2 green"]
parsePlays :: [String] -> [Play]
parsePlays playList = [parsePlay playItems | play <- playList, let playItems = splitOn ',' play]

parseGame :: String -> Game
parseGame xs = Game { gameId = gameId, plays = plays }
    where
        (id_:games_) = splitOn ':' xs
        gameId = read (last . words $ id_) :: Int
        plays = parsePlays $ splitOn ';' (head games_)

isValidPlay :: Play -> Play -> Bool
isValidPlay play bag = and [p <= b | (p, b) <- zip play bag]

isValidGame :: Game -> Play -> Bool
isValidGame game bag = and [isValidPlay play bag | play <- plays game]

validGameIds :: Play -> [Game] -> [Int]
validGameIds bag games = [gameId game | game <- games, isValidGame game bag]

transpose :: [[a]] -> [[a]]
transpose [] = []
transpose m = [[row !! i | row <- m] | i <- [0 .. length (head m) - 1]]

minSetGame :: [Play] -> Play
minSetGame plays = [maximum col | col <- transpose plays]

minSetOfCubes :: [Game] -> [Play]
minSetOfCubes games = [minSetGame (plays game) | game <- games]

main :: IO ()
main = do
    fileString <- readFile "input.txt"
    let bag = [12, 13, 14]
    -- part 1
    print . sum . validGameIds bag . map parseGame . lines $ fileString
    -- part 2
    print . sum . map product . minSetOfCubes . map parseGame . lines $ fileString
