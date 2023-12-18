import Data.List
import Debug.Trace
import qualified Data.Array as A

data Record = Record {
        springs :: String,
        damagedCounts :: [Int]
    } deriving (Show)

splitOn :: Char -> String -> [String]
splitOn _ [] = []
splitOn delim str = fs : splitOn delim ls
    where (fs, ls) = break (== delim) (dropWhile (== delim) str)

parseLine :: String -> Record
parseLine str = Record {
        springs = springs,
        damagedCounts = damagedCounts
    }
    where
        [springs, counts] = splitOn ' ' str
        damagedCounts = map read (splitOn ',' counts)

isValid :: Record -> Bool
isValid record = isValid' (springs record) (damagedCounts record)
    where
        isValid' str [] = null $ dropWhile (/= '#') str
        isValid' str (c:cs)
            | null damagedRemaining = False
            | length damaged /= c = False
            | otherwise = isValid' rest cs
            where
                damagedRemaining = dropWhile (/= '#') str
                (damaged, rest) = break (/= '#') damagedRemaining

complete :: Record -> Bool
complete record = '?' `notElem` springs record

addChar :: Char -> Record -> Record
addChar c record = Record {
        springs = let (fst,lst) = break (== '?') (springs record) in fst ++ [c] ++ tail lst,
        damagedCounts = damagedCounts record
    }

-- brute force
countArrangements :: Record -> Int
countArrangements record
    | complete record = if isValid record then 1 else 0
    | otherwise = countArrangements dotRecord + countArrangements tagRecord
    where
        dotRecord = addChar '.' record
        tagRecord = addChar '#' record

addDotIfPossible :: String -> String
addDotIfPossible [] = []
addDotIfPossible springs
    | head springs == '?' = '.' : tail springs
    | otherwise = springs

numAvailable :: String -> Int
numAvailable = length . takeWhile (/= '.')

-- memoization strategy taken from https://github.com/ColonelPhantom/aoc2023/blob/main/Day12.hs
countIncrementalArrangements :: Record -> Int
countIncrementalArrangements record = countIncMemo xss yss
    where
        xss = springs record
        yss = damagedCounts record
        springsLen = length xss
        countsLen = length yss

        -- map from dropped chars to result
        vals = A.array ((0,0), (springsLen, countsLen))
            [((x, y), countInc (drop x xss) (drop y yss)) | x <- [0..springsLen], y <- [0..countsLen]]

        countIncMemo :: String -> [Int] -> Int
        countIncMemo dx dy = vals A.! (springsLen - length dx, countsLen - length dy)

        countInc :: String -> [Int] -> Int
        countInc [] [] = 1
        countInc [] _ = 0
        countInc springs [] = if '#' `notElem` springs then 1 else 0 
        countInc springs@(s:ss) counts@(c:cs)
            | s == '.' = countIncMemo ss counts
            | s == '?' = countIncMemo ss counts + countInc ('#': ss) counts
            | lenCurDamaged > c = 0
            | maxDamagedLen == c = countIncMemo nextGroup cs
            | maxDamagedLen > c = if not (null nextGroup) && head nextGroup == '#'
                then 0 else countIncMemo (tail nextGroup) cs
            | otherwise = 0
            where
                (curDamaged, rest) = break (/= '#') springs
                lenCurDamaged = length curDamaged
                nextGroup = drop (c - lenCurDamaged) rest
                maxDamagedLen = numAvailable rest + lenCurDamaged


exploreArrangements :: Record -> [Record]
exploreArrangements record
    | complete record = [record]
    | otherwise = exploreArrangements dotRecord ++ exploreArrangements tagRecord
    where
        dotRecord = addChar '.' record
        tagRecord = addChar '#' record

unfoldRecord :: Record -> Record
unfoldRecord record = Record {
        springs = intercalate "?" $ replicate 5 beforeSprings,
        damagedCounts = concat $ replicate 5 beforeCounts
    }
    where
        beforeSprings = springs record
        beforeCounts = damagedCounts record

main :: IO ()
main = do
    fileString <- readFile "input.txt"
    -- part 1
    -- print . sum . map (countArrangements . parseLine) . lines $ fileString
    print . sum . map (countIncrementalArrangements . parseLine) . lines $ fileString
    -- part 2
    print . sum . map (countIncrementalArrangements . unfoldRecord . parseLine) . lines $ fileString
