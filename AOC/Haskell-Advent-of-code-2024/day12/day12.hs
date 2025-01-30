import Data.List (nub)
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set

-- Parse the input into a grid
parseMap :: String -> [[Char]]
parseMap = lines

-- Directions for checking neighbors (left, right, up, down)
neighbors :: Int -> Int -> [(Int, Int)]
neighbors x y = [(x-1, y), (x+1, y), (x, y-1), (x, y+1)]

-- Flood fill using DFS to get the region
floodFill :: [[Char]] -> Int -> Int -> Char -> Set.Set (Int, Int) -> [(Int, Int)]
floodFill grid x y plantType visited
  | x < 0 || y < 0 || x >= length grid || y >= length (head grid) = []
  | (x, y) `Set.member` visited = []
  | grid !! x !! y /= plantType = []
  | otherwise = (x, y) : concatMap (\(nx, ny) -> floodFill grid nx ny plantType (Set.insert (x, y) visited)) neighborsCoords
  where
    neighborsCoords = neighbors x y

-- Calculate the perimeter of a given region
calculatePerimeter :: [[Char]] -> [(Int, Int)] -> Int
calculatePerimeter grid region = length $ filter (isExposed grid region) allEdges
  where
    allEdges = [(x, y, nx, ny) | (x, y) <- region, (nx, ny) <- neighbors x y]
    isExposed grid region (x, y, nx, ny)
      | nx < 0 || ny < 0 || nx >= length grid || ny >= length (head grid) = True
      | (grid !! nx !! ny) /= (grid !! x !! y) = True
      | (nx, ny) `notElem` region = True
      | otherwise = False

-- Calculate area of a given region
calculateArea :: [(Int, Int)] -> Int
calculateArea = length

-- Given the grid, find all regions and compute the total cost
calculateTotalCost :: [[Char]] -> Int
calculateTotalCost grid = sum $ map regionCost regions
  where
    visited = Set.empty
    regions = findRegions grid visited
    regionCost region = calculateArea region * calculatePerimeter grid region

-- Find all regions by performing a flood fill
findRegions :: [[Char]] -> Set.Set (Int, Int) -> [[(Int, Int)]]
findRegions grid visited = [region | (x, y) <- allCoords, not (Set.member (x, y) visited), let region = floodFill grid x y (grid !! x !! y) visited]
  where
    allCoords = [(x, y) | x <- [0..length grid - 1], y <- [0..length (head grid) - 1]]


readMapFromFile :: FilePath -> IO [[Char]]
readMapFromFile filePath = do
    content <- readFile filePath
    return (parseMap content)


main :: IO ()
main = do

    let filePath = "day12/input.txt"
    grid <- readMapFromFile filePath
    print $ calculateTotalCost grid
