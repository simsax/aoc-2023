allEqual :: Eq a => [a] -> Bool
allEqual xs = all (== head xs) (tail xs)

predictNextValue :: [Int] -> Int
predictNextValue xs
    | allEqual xs = head xs
    | otherwise = last xs + predictNextValue (zipWith (-) (drop 1 xs) xs)

main :: IO ()
main = do
    fileString <- readFile "input.txt"
    -- part 1
    print . sum . map (predictNextValue . map (read :: String -> Int) . words) . lines $ fileString
    -- part 2
    print . sum . map (predictNextValue . reverse . map (read :: String -> Int) . words) . lines $ fileString
