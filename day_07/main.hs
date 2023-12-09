import Data.List
import Data.Ord

data HandType = HighCard | OnePair | TwoPair | ThreeOfAK | FullHouse | FourOfAK
    | FiveOfAK deriving (Show, Eq, Ord, Enum)

data Card = Joker | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten
            | Jack | Queen | King | Ace deriving (Show, Eq, Ord, Enum)

data Hand = Hand {
        cards :: [Card],
        bid :: Int,
        handType :: HandType
    } deriving (Show)

parseCard :: Char -> Card
parseCard c =
    case c of
       '2' -> Two
       '3' -> Three
       '4' -> Four
       '5' -> Five
       '6' -> Six
       '7' -> Seven
       '8' -> Eight
       '9' -> Nine
       'T' -> Ten
       'J' -> Jack
       'Q' -> Queen
       'K' -> King
       'A' -> Ace

parseCard2 :: Char -> Card
parseCard2 c =
    case c of
       '2' -> Two
       '3' -> Three
       '4' -> Four
       '5' -> Five
       '6' -> Six
       '7' -> Seven
       '8' -> Eight
       '9' -> Nine
       'T' -> Ten
       'J' -> Joker
       'Q' -> Queen
       'K' -> King
       'A' -> Ace


allEqual :: Eq a => [a] -> Bool
allEqual xs = all (== head xs) (tail xs)

checkFiveOfAK :: [Card] -> Bool
checkFiveOfAK = allEqual

checkFourOfAK :: [Card] -> Bool
checkFourOfAK cards = allEqual (init cards) || allEqual (tail cards)

checkFullHouse :: [Card] -> Bool
checkFullHouse cards =
    allEqual (take 3 cards) && allEqual (drop 3 cards) ||
    allEqual (take 2 cards) && allEqual (drop 2 cards)

checkThreeOfAK :: [Card] -> Bool
checkThreeOfAK cards@(c:cs)
    | length cards < 3 = False
    | otherwise = allEqual (take 3 cards) || checkThreeOfAK cs

-- X2233 22X33 2233X
checkTwoPair :: [Card] -> Bool
checkTwoPair cards = 
    allEqual (take 2 tailCards) && allEqual (drop 2 tailCards) ||
    allEqual (take 2 cards) && allEqual (drop 3 cards) ||
    allEqual (take 2 initCards) && allEqual (drop 2 initCards)
    where
        tailCards = tail cards
        initCards = init cards

checkOnePair :: [Card] -> Bool
checkOnePair cards@(c:cs)
    | length cards < 2 = False
    | otherwise = allEqual (take 2 cards) || checkOnePair cs

classifyHand :: [Card] -> HandType
classifyHand cards
    | checkFiveOfAK cards = FiveOfAK
    | checkFourOfAK sortedCards = FourOfAK
    | checkFullHouse sortedCards = FullHouse
    | checkThreeOfAK sortedCards = ThreeOfAK
    | checkTwoPair sortedCards = TwoPair
    | checkOnePair sortedCards = OnePair
    | otherwise = HighCard
    where
        sortedCards = sort cards

mostFrequentValue :: Eq a => [a] -> a
mostFrequentValue = head . last . sortOn length . group

replaceValue :: Eq a => a -> a -> [a] -> [a]
replaceValue bef aft = map (\x -> if x == bef then aft else x)

handleJoker :: [Card] -> [Card]
handleJoker cards
    | head cards /= Joker = cards
    | otherwise = replaceValue Joker mostOccCard cards
    where
        realCards = dropWhile (== Joker) cards
        mostOccCard = if null realCards then Joker else mostFrequentValue realCards

classifyHand2 :: [Card] -> HandType
classifyHand2 cards
    | checkFiveOfAK jokCards = FiveOfAK
    | checkFourOfAK jokCards = FourOfAK
    | checkFullHouse jokCards = FullHouse
    | checkThreeOfAK jokCards = ThreeOfAK
    | checkTwoPair jokCards = TwoPair
    | checkOnePair jokCards = OnePair
    | otherwise = HighCard
    where
        jokCards = sort . handleJoker . sort $ cards


parseLine :: String -> Hand
parseLine xs = Hand {
        cards = cards,
        bid = bid,
        handType = classifyHand cards
    }
    where
        [cardsStr, bidStr] = words xs
        bid = read bidStr :: Int
        cards = map parseCard cardsStr

parseLine2 :: String -> Hand
parseLine2 xs = Hand {
        cards = cards,
        bid = bid,
        handType = classifyHand2 cards
    }
    where
        [cardsStr, bidStr] = words xs
        bid = read bidStr :: Int
        cards = map parseCard2 cardsStr
        
        
sortHands :: [Hand] -> [Hand]
sortHands = sortOn (\hand -> (handType hand, cards hand))

scoreHands :: [Hand] -> [Int]
scoreHands hands = [i * b | (i, hand) <- zip [1..length hands] hands, let b = bid hand]

main :: IO ()
main = do
    fileString <- readFile "input.txt"
    -- part 1
    print . sum . scoreHands . sortHands . map parseLine . lines $ fileString
    -- part 2
    print . sum . scoreHands . sortHands . map parseLine2 . lines $ fileString

