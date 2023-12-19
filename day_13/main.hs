import Data.Maybe
import Data.List (transpose, find)

type Pattern = [String]

parsePatterns :: [String] -> [Pattern]
parsePatterns [] = []
parsePatterns xss =
    let (pattern, rest) = break null xss
    in pattern : if null rest then [] else parsePatterns (tail rest)

findReflRow :: Pattern -> Maybe Int -> Maybe Int
findReflRow pattern ignoreValue = findRefl' pattern ignoreValue 0
    where
        findRefl' pattern ignoreValue curRow
            | remRows == 0 = Nothing
            | curRow < remRows && Just winSize /= ignoreValue && take winSize pattern == reverse (take winSize (drop winSize pattern)) = Just winSize
            | curRow >= remRows && Just winSize /= ignoreValue && take remRows (reverse pattern) == reverse (take remRows (drop remRows (reverse pattern))) = Just winSize
            | otherwise = findRefl' pattern ignoreValue (curRow + 1)
            where
                lenRows = length pattern
                remRows = lenRows - 1 - curRow
                winSize = curRow + 1

findReflection :: Pattern -> Int
findReflection pattern = fromMaybe (if isJust rowReflection then fromJust rowReflection * 100 else 0) colReflection
    where
        rowReflection = findReflRow pattern Nothing
        colReflection = findReflRow (transpose pattern) Nothing

countDifferences :: Eq a => [a] -> [a] -> Int
countDifferences xs ys = length $ filter id $ zipWith (/=) xs ys

replacePatternRow :: Pattern -> Int -> String -> Pattern
replacePatternRow pattern index newRow = take index pattern ++ [newRow] ++ drop (index + 1) pattern 

tryNewPattern :: Pattern -> String -> [String] -> Int -> Maybe Pattern
tryNewPattern origPattern x [] _ = Nothing
tryNewPattern origPattern x (y:ys) ix
    | countDifferences x y == 1 = Just $ replacePatternRow origPattern ix y
    | otherwise = tryNewPattern origPattern x ys ix

findSmudgeReflectionRow :: Pattern -> Maybe Int -> Maybe Int
findSmudgeReflectionRow pattern origReflection = findSmudge' origReflection pattern pattern 0
    where 
        findSmudge' origReflection origPattern [] _ = Nothing
        findSmudge' origRelfection origPattern (p:ps) rowCount
            | isJust newPattern && isJust newRowReflection && newRowReflection /= origReflection = newRowReflection
            | otherwise = findSmudge' origReflection origPattern ps (rowCount + 1)
            where
                newPattern = tryNewPattern origPattern p ps rowCount
                newRowReflection = findReflRow (fromJust newPattern) origReflection

findSmudgeReflection :: Pattern -> Int
findSmudgeReflection pattern = fromMaybe (if isJust rowReflection then fromJust rowReflection * 100 else 0) colReflection
    where
        origRowReflection = findReflRow pattern Nothing
        origColReflection = findReflRow (transpose pattern) Nothing
        rowReflection = findSmudgeReflectionRow pattern origRowReflection
        colReflection = findSmudgeReflectionRow (transpose pattern) origColReflection


main :: IO ()
main = do
    fileString <- readFile "input.txt"
    -- part 1
    print . sum . map findReflection . parsePatterns . lines $ fileString
    -- part 2
    print . sum . map findSmudgeReflection . parsePatterns . lines $ fileString


