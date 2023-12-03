import Data.Char
import Data.Maybe

numbers = []

extractDigits :: String -> String
extractDigits xs = [x | x <- xs, isDigit x]

extractCalibration :: String -> Int
extractCalibration xs
    | digitsLength == 0 = 0
    | otherwise = read $ head digitString : [last digitString] :: Int
    where
        digitString = extractDigits xs
        digitsLength = length digitString
        
beginsWith :: String -> String -> Bool
beginsWith xs ys
    | null xs = False
    | length ys < length xs = False
    | xs == take (length xs) ys = True
    | otherwise = False

-- easy dumb approach, a more efficient solution would be to use a trie
wordsToNumbers :: String -> String
wordsToNumbers xs = wordsToNumber' xs numbers 1
    where
        numbers = ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]

        wordsToNumber' [] _ _ = []
        wordsToNumber' (x:xs) [] _ = x : wordsToNumber' xs numbers 1
        wordsToNumber' str@(x:xs) (num:nums) i
            | beginsWith num str = show i ++ wordsToNumber' (drop (length num - 1) str) numbers 1
            | otherwise = wordsToNumber' str nums (i + 1)


main :: IO ()
main = do
    fileString <- readFile "input.txt"
    print . sum . map (extractCalibration . wordsToNumbers) . lines $ fileString
