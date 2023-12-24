import Data.Char (ord, isLetter, digitToInt)

type Value = (String, Int)
type HashMap = [[Value]]
data Operation = Remove | Set Int deriving (Show)

splitOn :: Char -> String -> [String]
splitOn _ [] = []
splitOn delim str = fs : splitOn delim ls
    where (fs, ls) = break (== delim) (dropWhile (== delim) str)

parseSequence :: String -> [String]
parseSequence = splitOn ',' . head . lines

calcHash :: String -> Int
calcHash str = calcHash' str 0
    where
        calcHash' [] curVal = curVal
        calcHash' (x:xs) curVal = calcHash' xs newVal
            where
                newVal = (curVal + ord x) * 17 `rem` 256

extractLabel :: String -> String
extractLabel = takeWhile isLetter

parseElement :: String -> (String, Operation)
parseElement str = (label, operation)
    where
        (label, rest) = span isLetter str
        operation = if head rest == '-' then Remove else Set (digitToInt (last rest))

mapInit :: HashMap
mapInit = [[] | i <- [0 .. 255]]

removeValue :: String -> [Value] -> [Value]
removeValue _ [] = []
removeValue label (x:xs)
    | fst x == label = xs
    | otherwise = x : removeValue label xs

setValue :: String -> Int -> [Value] -> [Value]
setValue label val [] = [(label, val)]
setValue label val (x:xs)
    | fst x == label = (label, val) : xs
    | otherwise = x : setValue label val xs


mapRemove :: String -> HashMap -> HashMap
mapRemove label hashMap = mapRemove' label (calcHash label) 0 hashMap []
    where
        mapRemove' :: String -> Int -> Int -> HashMap -> HashMap -> HashMap
        mapRemove' _ _ _ [] newMap = newMap
        mapRemove' label labelIndex mapIndex (ms:mss) newMap
            | labelIndex == mapIndex = newMap ++ [removeValue label ms] ++ mss
            | otherwise = mapRemove' label labelIndex (mapIndex + 1) mss (newMap ++ [ms])

mapSet :: String -> Int -> HashMap -> HashMap
mapSet label val hashMap = mapSet' label (calcHash label) 0 val hashMap []
    where
        mapSet' :: String -> Int -> Int -> Int -> HashMap -> HashMap -> HashMap
        mapSet' _ _ _ _ [] newMap = newMap
        mapSet' label labelIndex mapIndex val (ms:mss) newMap
            | labelIndex == mapIndex = newMap ++ [setValue label val ms] ++ mss
            | otherwise = mapSet' label labelIndex (mapIndex + 1) val mss (newMap ++ [ms])

makeMap :: [(String, Operation)] -> HashMap
makeMap elements = makeMap' elements mapInit
    where
        makeMap' [] hashMap = hashMap
        makeMap' ((label,op):xs) hashMap =
            case op of
                Set n -> makeMap' xs (mapSet label n hashMap)
                Remove -> makeMap' xs (mapRemove label hashMap)

boxFocusingPower :: [Value] -> Int -> Int
boxFocusingPower values boxIndex = boxFocusingPower' values boxIndex 1
    where
        boxFocusingPower' [] _ _ = 0
        boxFocusingPower' (x:xs) boxIndex slotIndex =
            (boxIndex + 1) * slotIndex * snd x + boxFocusingPower' xs boxIndex (slotIndex + 1)

totalFocusingPower :: HashMap -> Int
totalFocusingPower hashMap = totalFocusingPower' hashMap 0 0
    where
        totalFocusingPower' :: HashMap -> Int -> Int -> Int
        totalFocusingPower' [] _ acc = acc
        totalFocusingPower' (ms:mss) index acc
            | null ms = totalFocusingPower' mss (index + 1) acc
            | otherwise = totalFocusingPower' mss (index + 1) (acc + boxFocusingPower ms index)

main :: IO ()
main = do
    fileString <- readFile "input.txt"
    -- part 1
    print . sum . map calcHash . parseSequence $ fileString
    -- part 2
    print . totalFocusingPower . makeMap . map parseElement . parseSequence $ fileString
