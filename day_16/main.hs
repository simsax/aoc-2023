import Prelude hiding (Left, Right)
import Debug.Trace
import qualified Data.Array as A
import Control.Arrow (ArrowChoice(right))

type Point = (Int, Int)
data Direction = Up | Down | Left | Right deriving (Eq, Show)

data Tile = Tile {
        content :: Char,
        visited :: [Direction]
    } deriving (Show)

data Node = Node {
        coord :: Point,
        direction :: Direction
    } deriving (Show)

type Contraption = A.Array (Int,Int) Tile

parseTile :: Char -> Tile
parseTile c = Tile { content = c, visited = [] }

parseContraption :: [String] -> Contraption
parseContraption lines' = A.array ((0, 0), (ySize - 1, xSize - 1)) [((i, j), parseTile (lines' !! i !! j)) | i <- [0..ySize-1], j <- [0..xSize-1]]
  where
    ySize = length lines'
    xSize = length (head lines')

calcCoord :: Int -> Int -> Int -> Int -> Direction -> Point
calcCoord xCoord yCoord xSize ySize Up = if yCoord > 0 then (xCoord, yCoord - 1) else (-1, -1)
calcCoord xCoord yCoord xSize ySize Down = if yCoord < ySize - 1 then (xCoord, yCoord + 1) else (-1, -1)
calcCoord xCoord yCoord xSize ySize Left = if xCoord > 0 then (xCoord - 1, yCoord) else (-1, -1)
calcCoord xCoord yCoord xSize ySize Right = if xCoord < xSize - 1 then (xCoord + 1, yCoord) else (-1, -1)

calcCoords :: Int -> Int -> Int -> Int -> [Direction] -> [Point]
calcCoords xCoord yCoord xSize ySize directions =
    [calcCoord xCoord yCoord xSize ySize direction | direction <- directions]

calcDirections :: Char -> Direction -> [Direction]
calcDirections '.' prevDirection = [prevDirection]
calcDirections '/' prevDirection = 
    case prevDirection of
        Up -> [Right]
        Down -> [Left]
        Left -> [Down]
        Right -> [Up]
calcDirections '\\' prevDirection = 
    case prevDirection of
        Up -> [Left]
        Down -> [Right]
        Left -> [Up]
        Right -> [Down]
calcDirections '-' prevDirection = 
    case prevDirection of
        Left -> [Left]
        Right -> [Right]
        _ -> [Left, Right]
calcDirections '|' prevDirection = 
    case prevDirection of
        Up -> [Up]
        Down -> [Down]
        _ -> [Up, Down]

markVisited :: Int -> Int -> Int -> Int -> [Direction] -> Contraption -> Contraption
markVisited xCoord yCoord xSize ySize directions contraption =
  contraption A.// [((yCoord, xCoord), Tile { content = content $ contraption A.! (yCoord, xCoord), visited = directions })]

bfs :: Contraption -> [Node] -> Int
bfs contraption [] = 0
bfs contraption (n:ns)
    | isVisited = bfs contraption ns
    | otherwise = (if isNotEnergized then 1 else 0) + bfs (markVisited xCoord yCoord xSize ySize (curDirection : visitedDirections) contraption) newNodes
    where
        xCoord = fst $ coord n
        yCoord = snd $ coord n
        curDirection = direction n
        curTile = contraption A.! (yCoord ,xCoord)
        visitedDirections = visited curTile
        isVisited = curDirection `elem` visitedDirections
        isNotEnergized = null visitedDirections
        curContent = content curTile
        sizes = snd $ A.bounds contraption
        ySize = fst sizes + 1
        xSize = snd sizes + 1
        nextDirections = calcDirections curContent curDirection
        nextCoords = calcCoords xCoord yCoord xSize ySize nextDirections
        newNodes = ns ++ [Node { coord = coord, direction = direction } |
            (coord, direction) <- zip nextCoords nextDirections, coord /= (-1, -1)]

countEnergizedTiles :: Contraption -> Int
countEnergizedTiles contraption = bfs contraption [Node { coord = (0, 0), direction = Right }]

countAllConfigurations :: Contraption -> [Int]
countAllConfigurations contraption = map (\node -> bfs contraption [node]) nodes
    where
        sizes = snd $ A.bounds contraption
        ySize = fst sizes + 1
        xSize = snd sizes + 1
        topCoords = [(x, 0) | x <- [0 .. xSize - 1]]
        topNodes = map (\c -> Node { coord = c, direction = Down }) topCoords
        bottomCoords = [(x, ySize - 1) | x <- [0 .. xSize - 1]]
        bottomNodes = map (\c -> Node { coord = c, direction = Up }) bottomCoords
        leftCoords = [(0, y) | y <- [0 .. ySize - 1]]
        leftNodes = map (\c -> Node { coord = c, direction = Right }) leftCoords
        rightCoords = [(xSize - 1, y) | y <- [0 .. ySize - 1]]
        rightNodes = map (\c -> Node { coord = c, direction = Left }) rightCoords
        nodes = topNodes ++ bottomNodes ++ leftNodes ++ rightNodes

getEnergized :: [Point] -> [String]
getEnergized points = [[if (x,y) `elem` points then '#' else '.' | x <- [0..size - 1]]| y <- [0..size-1]]
    where
        size = 10

main :: IO ()
main = do
    fileString <- readFile "input.txt"
    -- part 1
    print . countEnergizedTiles . parseContraption . lines $ fileString
    -- part 2
    print . maximum . countAllConfigurations . parseContraption . lines $ fileString

