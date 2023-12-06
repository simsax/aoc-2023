data Sheet = Sheet {
        times :: [Int], 
        distances :: [Int]
    } deriving (Show)

data Sheet2 = Sheet2 {
        time :: Int, 
        distance :: Int
    } deriving (Show)


splitOn :: Char -> String -> [String]
splitOn _ [] = []
splitOn delim str = fs : splitOn delim ls
    where (fs, ls) = break (== delim) (dropWhile (== delim) str)

parseSheet :: [String] -> Sheet
parseSheet [timeLine, distanceLine] = Sheet {
        times = map read . words . last . splitOn ':' $ timeLine,
        distances = map read . words . last . splitOn ':' $ distanceLine
    }

parseSheet2 :: [String] -> Sheet2
parseSheet2 [timeLine, distanceLine] = Sheet2 {
        time = read . concat . words . last . splitOn ':' $ timeLine,
        distance = read . concat . words . last . splitOn ':' $ distanceLine
    }

-- brute force
eval :: Int -> Int -> Int
eval t d = length (filter id [(t - hold) * hold > d | hold <- [0 .. t]])

isSquare :: Int -> Bool
isSquare n = sq * sq == n
    where
        sq = floor . sqrt $ fromIntegral n

-- exclude extremes when numbers are integers
myFloor :: RealFrac a => a -> Int
myFloor x
    | x == fromIntegral (floor x) = round x - 1
    | otherwise = fromIntegral (floor x)

myCeil :: RealFrac a => a -> Int
myCeil x
    | x == fromIntegral (ceiling x) = round x + 1
    | otherwise = fromIntegral (ceiling x)

-- quadratic formula
evalDirect :: Int -> Int -> Int
evalDirect  t d = let
    ft = fromIntegral t
    fd = fromIntegral d
    temp = sqrt $ ft**2 - 4 * fd
    x1 = myCeil $ (ft - temp) / 2
    x2 = myFloor $ (ft + temp) / 2
    in x2 - x1 + 1


waysToBeat :: Sheet -> [Int]
waysToBeat sheet = waysToBeat' (times sheet) (distances sheet)
    where
        waysToBeat' [] _ = []
        waysToBeat' _ [] = []
        waysToBeat' (t:ts) (d:ds) = evalDirect t d : waysToBeat' ts ds

waysToBeat2 :: Sheet2 -> Int
waysToBeat2 sheet = evalDirect (time sheet) (distance sheet)

main :: IO ()
main = do
    fileString <- readFile "input.txt"
    -- part 1
    print . product . waysToBeat . parseSheet . lines $ fileString
    -- part 2
    print . waysToBeat2 . parseSheet2 . lines $ fileString
