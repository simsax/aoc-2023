import Data.List

type Coord = (Int, Int)

allDots :: String -> Bool
allDots = all (== '.')

-- brute force
expandMap :: Int -> [String] -> [String]
expandMap count [] = []
expandMap count (xs:xss)
    | allDots xs = replicate count xs ++ expandMap count xss
    | otherwise = xs : expandMap count xss

expandUniverse :: Int -> [String] -> [String]
expandUniverse count = transpose . expandMap count . transpose . expandMap count

shortestPath :: Coord -> Coord -> Int
shortestPath (x1, y1) (x2, y2) = abs (x2 - x1) + abs (y2 - y1)

findGalaxies :: [String] -> [Coord]
findGalaxies xss = [(i, j) | i <- [0 .. length xss - 1], j <- [0 .. length (head xss) - 1], 
             xss !! i !! j == '#']

sumShortestPaths :: [Coord] -> Int
sumShortestPaths coords = sum [shortestPath x y | (x, ys) <- zip coords (tail (tails coords)), y <- ys]

findEmptyRows :: [String] -> [Int]
findEmptyRows xss = [i | i <- [0 .. length xss - 1], allDots (xss !! i)]

findEmptyCols :: [String] -> [Int]
findEmptyCols xss = [i | i <- [0 .. length txss - 1], allDots (txss !! i)]
    where
        txss = transpose xss

expandRows :: Int -> [Coord] -> [Int] -> [Coord]
expandRows count startCoords [] = startCoords
expandRows count startCoords (row:rows) = expandRows count updatedCoords rows
    where
        updatedCoords = [if fst coord > row then (fst coord + count, snd coord) else coord | coord <- startCoords]

expandCols :: Int -> [Coord] -> [Int] -> [Coord]
expandCols count startCoords [] = startCoords
expandCols count startCoords (col:cols) = expandCols count updatedCoords cols
    where
        updatedCoords = [if snd coord > col then (fst coord, snd coord + count) else coord | coord <- startCoords]

cumulativeSum :: Int -> [Int] -> [Int]
cumulativeSum count rows = [row + i * count | (i, row) <- zip [0 .. length rows - 1] rows]

expandAndFindGalaxies :: Int -> [String] -> [Coord]
expandAndFindGalaxies count xss = expandCols count (expandRows count startingGalaxies emptyRows) emptyCols
    where
        startingGalaxies = findGalaxies xss
        emptyRows = cumulativeSum count (findEmptyRows xss)
        emptyCols = cumulativeSum count (findEmptyCols xss)

main = do
    fileString <- readFile "input.txt"
    -- part 1
    print . sumShortestPaths . findGalaxies . expandUniverse 2 . lines $ fileString
    -- part 2
    print . sumShortestPaths . expandAndFindGalaxies (1000000 - 1) . lines $ fileString
