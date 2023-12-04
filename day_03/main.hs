import Data.Char hiding (isSymbol)
import Data.Maybe
import Data.List (sortOn, groupBy)

type Schematic = [[Char]]
data StarNumber = StarNumber { number :: Int, starRow :: Int, starCol :: Int } deriving (Show)

isDot :: Char -> Bool
isDot c = c == '.'

isStar :: Char -> Bool
isStar c = c == '*'

isSymbol :: Char -> Bool
isSymbol c = not (isDot c) && not (isDigit c)

toInt :: String -> Int
toInt s = read s :: Int

isNotDigit :: Char -> Bool
isNotDigit c = not (isDigit c)

findNum :: String -> Int -> String
findNum str index = takeWhile isDigit (drop index str)

findDigitIndex :: String -> Maybe Int
findDigitIndex str = findDigitIndex' str 0
    where
        findDigitIndex' [] _ = Nothing
        findDigitIndex' (x:xs) i
            | isDigit x = Just i
            | otherwise = findDigitIndex' xs (i + 1)

-- returns a tuple with the first occurence of a num with its starting index
firstNum :: String -> (String, Int)
firstNum str =
    case findDigitIndex str of
        Nothing -> ([], -1)
        Just index -> (findNum str index, index)

allNums :: String -> [(String, Int)]
allNums str = allNums' str 0
    where
        allNums' [] _ = []
        allNums' str@(x:xs) cumIndex
            | isNotDigit x = allNums' xs (cumIndex + 1) 
            | index == -1 = []
            | otherwise = (num, cumIndex + index) : allNums' (drop (index + length num) str) newCumIndex
            where
                (num, index) = firstNum str
                newCumIndex = cumIndex + index + length num

rowLength :: Schematic -> Int
rowLength schema = length (head schema)

colLength :: Schematic -> Int
colLength schema = length [head row | row <- schema]

checkCellSymbol :: Int -> Int -> Schematic -> Bool
checkCellSymbol row col schema
    | col < 0 || col >= rowLength schema  = False
    | otherwise = isSymbol (schema !! row !! col)

findLineSymbol :: Int -> Int -> Int -> Schematic -> Bool
findLineSymbol _ _ 0 _ = False
findLineSymbol row col lineLength schema
    | col < 0 = findLineSymbol row 0 (lineLength - 1) schema
    | col >= rowLength schema = False
    | isSymbol (schema !! row !! col) = True
    | otherwise = findLineSymbol row (col + 1) (lineLength - 1) schema

checkLineSymbol :: Int -> Int -> Int -> Schematic -> Bool
checkLineSymbol row col lineLength schema
    | row < 0 || row >= colLength schema = False
    | otherwise = findLineSymbol row col lineLength schema

isPartNumber :: Int -> String -> (String, Int) -> Schematic -> Int
isPartNumber rowIndex row (num, colIndex) schema
    | checkCellSymbol rowIndex (colIndex - 1) schema ||
      checkCellSymbol rowIndex (colIndex + length num) schema ||
      checkLineSymbol (rowIndex - 1) (colIndex - 1) (length num + 2) schema ||
      checkLineSymbol (rowIndex + 1) (colIndex - 1) (length num + 2) schema = toInt num
    | otherwise = 0


findPartNumbers :: [String] -> [Int]
findPartNumbers schema = concat [[isPartNumber rowIndex row num schema | num <- allNums row] | (rowIndex, row) <- zip [0 .. rowLength schema - 1] schema]

checkCellStar :: Int -> Int -> Schematic -> Maybe (Int, Int)
checkCellStar row col schema
    | col < 0 || col >= rowLength schema  = Nothing
    | otherwise = if isStar (schema !! row !! col) then Just (row, col) else Nothing

findLineStar :: Int -> Int -> Int -> Schematic -> Maybe (Int, Int)
findLineStar _ _ 0 _ = Nothing
findLineStar row col lineLength schema
    | col < 0 = findLineStar row 0 (lineLength - 1) schema
    | col >= rowLength schema = Nothing
    | isStar (schema !! row !! col) = Just (row, col)
    | otherwise = findLineStar row (col + 1) (lineLength - 1) schema

checkLineStar :: Int -> Int -> Int -> Schematic -> Maybe (Int, Int)
checkLineStar row col lineLength schema
    | row < 0 || row >= colLength schema = Nothing
    | otherwise = findLineStar row col lineLength schema

isGearNumber :: Int -> String -> (String, Int) -> Schematic -> Maybe StarNumber
isGearNumber rowIndex row (num, colIndex) schema
    | isJust res1 = fmap (\(x, y) -> StarNumber { number = toInt num , starRow = x, starCol = y }) res1
    | isJust res2 = fmap (\(x, y) -> StarNumber { number = toInt num , starRow = x, starCol = y }) res2
    | isJust res3 = fmap (\(x, y) -> StarNumber { number = toInt num , starRow = x, starCol = y }) res3
    | isJust res4 = fmap (\(x, y) -> StarNumber { number = toInt num , starRow = x, starCol = y }) res4
    | otherwise = Nothing
    where
        res1 = checkCellStar rowIndex (colIndex - 1) schema
        res2 = checkCellStar rowIndex (colIndex + length num) schema
        res3 = checkLineStar (rowIndex - 1) (colIndex - 1) (length num + 2) schema
        res4 = checkLineStar (rowIndex + 1) (colIndex - 1) (length num + 2) schema

findStarNumbers :: [String] -> [StarNumber]
findStarNumbers schema = catMaybes $ concat [[isGearNumber rowIndex row num schema | num <- allNums row] | (rowIndex, row) <- zip [0 .. rowLength schema - 1] schema]

groupStarNumbers :: [StarNumber] -> [[StarNumber]]
groupStarNumbers starNums =
  groupBy (\star1 star2 -> starRow star1 == starRow star2 && starCol star1 == starCol star2) $
  sortOn (\starNum -> (starRow starNum, starCol starNum)) starNums

filterGearNumbers :: [[StarNumber]] -> [[StarNumber]]
filterGearNumbers = filter (\nums -> length nums == 2)

calcGearRatios :: [[StarNumber]] -> [Int]
calcGearRatios starNums = [product . map number $ row | row <- starNums]

main :: IO ()
main = do
    fileString <- readFile "input.txt"
    -- part 1
    print . sum . findPartNumbers . lines $ fileString
    -- part 2
    print . sum . calcGearRatios . filterGearNumbers . groupStarNumbers . findStarNumbers . lines $ fileString
