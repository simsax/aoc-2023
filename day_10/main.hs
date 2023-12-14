import Data.List
import Data.Maybe
import Prelude hiding (Left, Right)

data Tile = Start | Ground | UpDown | LeftRight | UpRight | UpLeft | DownRight | DownLeft deriving (Eq, Show)
data Direction = Up | Down | Left | Right deriving (Eq, Show)
type Coord = (Int, Int)

parseTile :: String -> [Tile]
parseTile [] = []
parseTile (x:xs) =
    case x of
        'S' -> Start : parseTile xs
        '.' -> Ground : parseTile xs
        '|' -> UpDown : parseTile xs
        '-' -> LeftRight : parseTile xs
        'L' -> UpRight : parseTile xs
        'J' -> UpLeft : parseTile xs
        'F' -> DownRight : parseTile xs
        '7' -> DownLeft : parseTile xs

upOk :: Tile -> Bool
upOk UpDown = True
upOk DownRight = True
upOk DownLeft = True
upOk _ = False

downOk :: Tile -> Bool
downOk UpDown = True
downOk UpRight = True
downOk UpLeft = True
downOk _ = False

rightOk :: Tile -> Bool
rightOk LeftRight = True
rightOk UpLeft = True
rightOk DownLeft = True
rightOk _ = False

leftOk :: Tile -> Bool
leftOk LeftRight = True
leftOk UpRight = True
leftOk DownRight = True
leftOk _ = False

findStartCol :: [Tile] -> Maybe Int
findStartCol = elemIndex Start

findStart :: [[Tile]] -> Coord
findStart tss = findStart' 0 tss
    where 
        findStart' _ [] = (-1, -1)
        findStart' row (ts:tss)
            | isJust col = (row, fromJust col)
            | otherwise = findStart' (row + 1) tss
            where
                col = findStartCol ts

goUp :: Coord -> Coord
goUp (row, col) = (row - 1, col)

goDown :: Coord -> Coord
goDown (row, col) = (row + 1, col)

goLeft :: Coord -> Coord
goLeft (row, col) = (row, col - 1)

goRight :: Coord -> Coord
goRight (row, col) = (row, col + 1)

evaluateTile :: Tile -> Direction -> Coord -> (Direction, Coord)
evaluateTile Start direction rowCol = (direction, rowCol)
evaluateTile Ground direction rowCol = (direction, rowCol)
evaluateTile UpDown direction rowCol
    | direction == Up = (Up, goUp rowCol)
    | otherwise = (Down, goDown rowCol)
evaluateTile LeftRight direction rowCol
    | direction == Left = (Left, goLeft rowCol)
    | otherwise = (Right, goRight rowCol)
evaluateTile UpRight direction rowCol
    | direction == Down = (Right, goRight rowCol)
    | otherwise = (Up, goUp rowCol)
evaluateTile UpLeft direction rowCol
    | direction == Down = (Left, goLeft rowCol)
    | otherwise = (Up, goUp rowCol)
evaluateTile DownRight direction rowCol
    | direction == Up = (Right, goRight rowCol)
    | otherwise = (Down, goDown rowCol)
evaluateTile DownLeft direction rowCol
    | direction == Up = (Left, goLeft rowCol)
    | otherwise = (Down, goDown rowCol)

step :: [[Tile]] -> Direction -> Int -> Int -> [Coord] -> [Coord]
step tiles direction row col loopTiles
    | tile == Start = loopTiles
    | otherwise = step tiles nextDirection nextRow nextCol (loopTiles ++ [(row, col)])
    where
        tile = tiles !! row !! col
        (nextDirection, (nextRow, nextCol)) = evaluateTile tile direction (row, col)

rowLength :: [[Tile]] -> Int
rowLength tiles = length (head tiles)

colLength :: [[Tile]] -> Int
colLength tiles = length [head (tiles !! i) | i <- [0 .. rowLength tiles - 1]]

findLoopTiles :: [[Tile]] -> [Coord]
findLoopTiles tiles
    | upOk tileUp = step tiles Up (row - 1) col [(row, col)]
    | rightOk tileRight = step tiles Right row (col + 1) [(row, col)]
    | downOk tileDown = step tiles Down (row + 1) col [(row, col)]
    | leftOk tileLeft = step tiles Left row (col - 1) [(row, col)]
    where
        startRowCol = findStart tiles
        row = fst startRowCol
        col = snd startRowCol
        tileUp = if row == 0 then Ground else tiles !! (row - 1) !! col
        tileRight = if col == (rowLength tiles - 1) then Ground else tiles !! row !! (col + 1)
        tileDown = if row == (colLength tiles - 1) then Ground else tiles !! (row + 1) !! col
        tileLeft = if col == 0 then Ground else tiles !! row !! (col - 1)

isInside :: [[Tile]] -> [Coord] -> Int -> Coord -> Bool
isInside tiles loopTiles colLength tile = odd (countIntersection tiles loopTiles colLength tile Ground 0)
    where
        countIntersection tiles loopTiles colLength tile prevCorner count
            | tileCol == colLength = count
            | isLoopTile && curTile == LeftRight =
                countIntersection tiles loopTiles colLength (tileRow, tileCol + 1) prevCorner count 
            | isLoopTile && (curTile == DownRight || curTile == UpRight) = 
                countIntersection tiles loopTiles colLength (tileRow, tileCol + 1) curTile (count + 1)
            | isLoopTile && curTile == DownLeft && prevCorner == DownRight =
                countIntersection tiles loopTiles colLength (tileRow, tileCol + 1) curTile (count + 1)
            | isLoopTile && curTile == DownLeft && prevCorner == UpRight =
                countIntersection tiles loopTiles colLength (tileRow, tileCol + 1) curTile count
            | isLoopTile && curTile == UpLeft && prevCorner == UpRight =
                countIntersection tiles loopTiles colLength (tileRow, tileCol + 1) curTile (count + 1)
            | isLoopTile && curTile == UpLeft && prevCorner == DownRight =
                countIntersection tiles loopTiles colLength (tileRow, tileCol + 1) curTile count
            | otherwise = countIntersection tiles loopTiles colLength (tileRow, tileCol + 1) prevCorner
                (if isLoopTile then count + 1 else count) 
            where
                tileRow = fst tile
                tileCol = snd tile
                curTile = tiles !! tileRow !! tileCol
                isLoopTile = elem tile loopTiles

countInsideLoop :: [[Tile]] -> Int
countInsideLoop tiles = length (filter (isInside tiles loopTiles colLength) freeTiles)
-- countInsideLoop :: [[Tile]] -> [Coord]
-- countInsideLoop tiles = filter (isInside tiles loopTiles colLength) freeTiles
    where
        colLength = length (head tiles)
        loopTiles = findLoopTiles tiles
        freeTiles = [(i,j) | i <- [0 .. length tiles - 1], j <- [0 .. length (head tiles) - 1]] \\ loopTiles

main :: IO ()
main = do
    fileString <- readFile "input.txt"
    -- part 1
    print . ceiling . (/ 2) . fromIntegral . length . findLoopTiles . map parseTile . lines $ fileString
    -- part 2
    print . countInsideLoop . map parseTile . lines $ fileString
