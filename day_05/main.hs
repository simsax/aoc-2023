data Map = Map {
        sourceStart :: Int,
        destStart :: Int,
        range:: Int,
        sourceEnd :: Int,
        offset :: Int
    } deriving (Show)

data Almanac = Almanac {
        seeds :: [Int],
        maps :: [[Map]]
    } deriving (Show)

splitOn :: Char -> String -> [String]
splitOn _ [] = []
splitOn delim str = fs : splitOn delim ls
    where (fs, ls) = break (== delim) (dropWhile (== delim) str)

parseSeeds :: String -> [Int]
parseSeeds seedstr = map read . words . last $ splitOn ':' seedstr

explodeSeeds :: [Int] -> [Int]
explodeSeeds [] = []
explodeSeeds (start:range:seeds) = [start .. start + range - 1] ++ explodeSeeds seeds

parseMap :: [Int] -> Map
parseMap [dest, source, range] = Map {
        sourceStart = source,
        destStart = dest,
        range = range,
        sourceEnd = source + range - 1,
        offset = dest - source
    }

parseMaps :: [String] -> [[Map]]
parseMaps [] = []
parseMaps maps@(m:ms)
    | last m == ':' = parseMaps ms
    | otherwise = map (parseMap . map read . words) currentMaps : parseMaps (drop (length currentMaps) ms)
    where
        currentMaps = takeWhile (\x -> last x /= ':') maps

parseAlmanac :: [String] -> Almanac
parseAlmanac (seeds:maps) = Almanac {
        seeds = parseSeeds seeds,
        maps = parseMaps maps
    }

parseAlmanac2 :: [String] -> Almanac
parseAlmanac2 (seeds:maps) = Almanac {
        seeds = explodeSeeds (parseSeeds seeds),
        maps = parseMaps maps
    }

mapSeed :: Int -> [Map] -> Int
mapSeed seed [] = seed
mapSeed seed (m:ms)
    | seed >= sourceStart m && seed <= sourceEnd m = seed + offset m
    | otherwise = mapSeed seed ms

findLocation :: Int -> [[Map]] -> Int
findLocation = foldl mapSeed
-- findLocation seed [] = seed
-- findLocation seed (ms:mss) = findLocation (mapSeed seed ms) mss

findLocations :: Almanac -> [Int]
findLocations almanac = findLocations' (seeds almanac) (maps almanac)
    where 
        findLocations' [] _ = []
        findLocations' (x:xs) maps = findLocation x maps : findLocations' xs maps


main :: IO ()
main = do
    fileString <- readFile "input.txt"
    -- part 1
    print . minimum . findLocations . parseAlmanac . filter (/="") . lines $ fileString
    -- part 2
    print . minimum . findLocations . parseAlmanac2 . filter (/="") . lines $ fileString

