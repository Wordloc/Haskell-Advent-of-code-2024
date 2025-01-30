import qualified Data.Map.Strict as Map
import Control.Monad (guard, replicateM)
import Data.List (nub)

-- Create a map from coordinates (row, col) to the corresponding grid value.
makeGrid :: [[Char]] -> Map.Map (Int, Int) Char
makeGrid grid = Map.fromList [((row, col), val) | (row, rowVals) <- zip [0..] grid, (col, val) <- zip [0..] rowVals]

-- Find all antinodes: positions with matching characters at opposite ends of a candidate pair.
getAntinodes :: Map.Map (Int, Int) Char -> [(Int, Int)]
getAntinodes grid = 
    let step (dr, dc) (r, c) = (r + dr, c + dc)
        candidates (r1, c1) (r2, c2) = 
            let (dr, dc) = (r2 - r1, c2 - c1)
            in [step (dr, dc) (r2, c2), step (-dr, -dc) (r1, c1)]
    in do
        [(p1, v1), (p2, v2)] <- replicateM 2 (Map.toList (Map.filter (/= '.') grid))
        guard $ p1 /= p2 && p1 < p2 && v1 == v2
        filter (flip Map.member grid) $ candidates p1 p2

-- Find antinodes considering an extended set of positions.
getAntinodespt2 :: Map.Map (Int, Int) Char -> [(Int, Int)]
getAntinodespt2 grid = 
    let step (dr, dc) (r, c) = (r + dr, c + dc)
        candidates (r1, c1) (r2, c2) = 
            let (dr, dc) = (r2 - r1, c2 - c1)
            in (r2, c2) : iterate (step (dr, dc)) (r2, c2)
    in do
        [(p1, v1), (p2, v2)] <- replicateM 2 (Map.toList (Map.filter (/= '.') grid))
        guard $ p1 /= p2 && v1 == v2
        takeWhile (flip Map.member grid) $ candidates p1 p2

-- Main function to read the input file and process the grid
main :: IO ()
main = do
    input <- lines <$> readFile "day08/input.txt"
    let grid = makeGrid input
    let antinodes = getAntinodes grid
    print $ length $ nub antinodes
    let antinodespt2 = getAntinodespt2 grid
    print $ length $ nub antinodespt2
