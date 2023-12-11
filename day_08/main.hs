import Prelude hiding (Left, Right)
import qualified Data.Map.Strict as Map

data Direction = Left | Right deriving (Eq, Show)

data Document = Document {
        directions :: [Direction],
        nodes :: Map.Map String (String, String),
        startKeys :: [String]
    } deriving (Show)


splitOn :: Char -> String -> [String]
splitOn _ [] = []
splitOn delim str = fs : splitOn delim ls
    where (fs, ls) = break (== delim) (dropWhile (== delim) str)

parseDirections :: String -> [Direction]
parseDirections = map (\x -> if x == 'L' then Left else Right)

parseNode :: String -> (String, (String, String))
parseNode xs = (key, val)
    where
        [keyStr,valStr] = splitOn '=' xs
        key = head (words keyStr)
        items = splitOn ',' (init (drop 2 valStr))
        val = (head items, tail . last $ items)

parseNodes :: [String] -> Map.Map String (String, String)
parseNodes xss = Map.fromList (map parseNode xss)

parseDocument :: [String] -> Document
parseDocument lines = Document {
        directions = directions,
        nodes = nodes,
        startKeys = []
    }
    where
        directions = parseDirections (head lines)
        nodes = parseNodes (drop 2 lines)

parseDocument2 :: [String] -> Document
parseDocument2 lines = Document {
        directions = directions,
        nodes = nodes,
        startKeys = startKeys
    }
    where
        directions = parseDirections (head lines)
        nodeList = map parseNode (drop 2 lines)
        startKeys = filter (\key -> last key == 'A') (map fst nodeList)
        nodes = Map.fromList nodeList

navigate :: Document -> Int
navigate doc = navigate' "AAA" (directions doc) (nodes doc)
    where
        navigate' curKey [] nodesMap = navigate' curKey (directions doc) nodesMap
        navigate' curKey (d:ds) nodesMap
            | d == Left = if fst curVal == "ZZZ" then 1 else 1 + navigate' (fst curVal) ds nodesMap
            | otherwise = if snd curVal == "ZZZ" then 1 else 1 + navigate' (snd curVal) ds nodesMap
            where
                curVal = nodesMap Map.! curKey

navigate2 :: Document -> [Int]
navigate2 doc = [navigate' key (directions doc) (nodes doc) | key <- startKeys doc]
    where
        navigate' curKey [] nodesMap = navigate' curKey (directions doc) nodesMap
        navigate' curKey (d:ds) nodesMap
            | d == Left = if last (fst curVal) == 'Z' then 1 else 1 + navigate' (fst curVal) ds nodesMap
            | otherwise = if last (snd curVal) == 'Z' then 1 else 1 + navigate' (snd curVal) ds nodesMap
            where
                curVal = nodesMap Map.! curKey

lcmList :: [Int] -> Int
lcmList (x:y:xs)
    | null xs = lcm x y
    | otherwise = lcm x (lcmList (y:xs))

main :: IO ()
main = do
    fileString <- readFile "input.txt"
    -- part 1
    print . navigate . parseDocument . lines $ fileString
    -- part 2
    print . lcmList . navigate2 . parseDocument2 . lines $ fileString
