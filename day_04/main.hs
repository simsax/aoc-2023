import Data.List (intersect)

data Card = Card {
        cardId :: Int,
        winningNumbers :: [Int],
        cardNumbers :: [Int],
        matches :: Int 
    } deriving (Show)

splitOn :: Char -> String -> [String]
splitOn _ [] = []
splitOn delim str = fs : splitOn delim ls
    where (fs, ls) = break (== delim) (dropWhile (== delim) str)

toInt :: String -> Int
toInt s = read s :: Int

parseCard :: String -> Card
parseCard str = Card {
           cardId = cardId,
           winningNumbers = winLs,
           cardNumbers = cardLs,
           matches = length $ winLs `intersect` cardLs  
    }
    where 
        [card, nums] = splitOn ':' str
        cardId = toInt . last . words $ card 
        [winNums, cardNums] = splitOn '|' nums
        winLs = map toInt (words winNums)
        cardLs = map toInt (words cardNums)

calcCardPoints :: Card -> Int
calcCardPoints card = if matches card == 0 then 0 else 2 ^ (matches card - 1)

calcNumCards :: [Card] -> Int
calcNumCards [] = 0
calcNumCards (x:xs) = 1 + calcNumCards' (take numMatches xs) (drop numMatches xs) + calcNumCards xs
    where 
        numMatches = matches x
        calcNumCards' [] _ = 0
        calcNumCards' (m:ms) rest = 1 + calcNumCards' (take numMatches' rest') (drop numMatches' rest') + calcNumCards' ms rest
            where
                numMatches' = matches m
                rest' = ms ++ rest

main :: IO ()
main = do
    fileString <- readFile "input.txt"
    -- part 1
    print . sum . map (calcCardPoints . parseCard) . lines $ fileString
    -- part 2
    print . calcNumCards . map parseCard . lines $ fileString

