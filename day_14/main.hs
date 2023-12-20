import Data.List (transpose)
import Prelude hiding (cycle)

calculateLoad :: String -> Int
calculateLoad row = calculateLoad' row (length row) 0 0
    where
        calculateLoad' :: String -> Int -> Int -> Int -> Int
        calculateLoad' [] _ _ _ = 0
        calculateLoad' (x:xs) rowLength freeIndex curIndex
            | x == 'O' = (rowLength - freeIndex) + calculateLoad' xs rowLength (freeIndex + 1) (curIndex + 1)
            | x == '.' = calculateLoad' xs rowLength freeIndex (curIndex + 1)
            | otherwise = calculateLoad' xs rowLength (curIndex + 1) (curIndex + 1)

updateRow :: String -> Int -> Int -> String
updateRow row freeIndex curIndex = [if i == freeIndex then 'O' else
                                    if i == curIndex then '.' else
                                    row !! i | i <- [0 .. length row - 1]]

slideWestRow :: String -> String
slideWestRow row = slideWestRow' row 0 0 row
    where
        slideWestRow' [] _ _ newRow = newRow
        slideWestRow' (x:xs) freeIndex curIndex newRow
            | x == 'O' = slideWestRow' xs (freeIndex + 1) (curIndex + 1) (updateRow newRow freeIndex curIndex)
            | x == '.' = slideWestRow' xs freeIndex (curIndex + 1) newRow
            | otherwise = slideWestRow' xs (curIndex + 1) (curIndex + 1) newRow

slideWest :: [String] -> [String]
slideWest = map slideWestRow

slideNorth :: [String] -> [String]
slideNorth = transpose . slideWest . transpose

slideEast :: [String] -> [String]
slideEast = map (reverse . slideWestRow . reverse)

slideSouth :: [String] -> [String]
slideSouth = transpose . slideEast . transpose

cycle :: [String] -> [String]
cycle = slideEast . slideSouth . slideWest . slideNorth

calculateNorthLoad :: [String] -> Int
calculateNorthLoad rocksMap =
 sum [ sum [if (row !! j) == 'O' then val else 0 | let row = rocksMap !! i, j <- [0 .. length row - 1]] 
    | (i, val) <- zip [0 .. length rocksMap - 1] (reverse [1 .. length rocksMap])]

cycleForever :: [String] -> [[String]]
cycleForever rocksMap = rocksMap : cycleForever (cycle rocksMap)

cycleN :: Int -> [String] -> [String]
cycleN times rocksMap = cycleForever rocksMap !! times

main :: IO ()
main = do
    fileString <- readFile "input.txt"
    -- part 1
    print . sum . map calculateLoad . transpose . lines $ fileString
    -- part 2
    print . calculateNorthLoad . cycleN 1000 . lines $ fileString
