import Data.DisjointSet (DSet, singletonsIO, find, union)
import Data.Array.IO (IOUArray)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad (forM_)

-- Parse the input into a grid of garden plots
parseMap :: String -> [[Char]]
parseMap = lines

-- Create a disjoint set for each element in the grid (each plot)
initializeDisjointSet :: Int -> Int -> IO (DSet IOUArray)
initializeDisjointSet numRows numCols = singletonsIO (0, numRows * numCols - 1)

-- Calculate the index for a (x, y) position in a 2D grid
index :: Int -> Int -> Int -> Int
index numCols x y = x * numCols + y

-- Directions for checking neighbors (left, right, up, down)
neighbors :: Int -> Int -> [(Int, Int)]
neighbors x y = [(x-1, y), (x+1, y), (x, y-1), (x, y+1)]

-- Union the sets of adjacent garden plots of the same type
unionAdjacent :: [[Char]] -> DSet IOUArray -> Int -> Int -> Int -> Int -> IO ()
unionAdjacent grid dset numCols x y nx ny = do
  -- Check if both plots are within the bounds
  Control.Monad.when (nx >= 0 && nx < length grid && ny >= 0 && ny < length (head grid)) $ do
    -- If they are of the same plant type, union their sets
    Control.Monad.when (grid !! x !! y == grid !! nx !! ny) $ union dset (index numCols x y) (index numCols nx ny)

-- Traverse the grid and union adjacent cells with the same plant type
processGrid :: [[Char]] -> DSet IOUArray -> Int -> Int -> IO ()
processGrid grid dset numCols = forM_ [(x, y) | x <- [0..length grid - 1], y <- [0..length (head grid) - 1]] $ \(x, y) -> do
  -- Get neighbors and union adjacent garden plots if they have the same plant type
  forM_ (neighbors x y) $ \(nx, ny) -> unionAdjacent grid dset numCols x y nx ny

-- Find the regions and calculate their total cost (area * perimeter)
calculateTotalCost :: [[Char]] -> IO Int
calculateTotalCost grid = do
  let numRows = length grid
      numCols = length (head grid)
  -- Initialize the disjoint set for the grid
  dset <- initializeDisjointSet numRows numCols

  -- Process the grid to union adjacent plots
  processGrid grid dset numCols

  -- Now, we need to compute the cost for each region.
  -- First, get all unique root indices
  let indices = [0..numRows * numCols - 1]

  -- Find the roots of all the indices and group them by root
  regionRoots <- mapM (find dset) indices

  -- Group by root
  let regions = Map.fromListWith (++) [(root, [(x, y)]) | (root, (x, y)) <- zip regionRoots [(x, y) | x <- [0..numRows-1], y <- [0..numCols-1]]]

  -- Now we need to compute the area and perimeter for each region
  let regionCosts = map (\(root, region) -> regionCost grid region) (Map.toList regions)
  return (sum regionCosts)

-- Calculate the area and perimeter for a region
regionCost :: [[Char]] -> [(Int, Int)] -> Int
regionCost grid region = calculateArea region * calculatePerimeter grid region

-- Calculate the area (just the number of plots)
calculateArea :: [(Int, Int)] -> Int
calculateArea = length

-- Calculate the perimeter of a region
calculatePerimeter :: [[Char]] -> [(Int, Int)] -> Int
calculatePerimeter grid region = length $ filter (isExposed grid region) allEdges
  where
    allEdges = [(x, y, nx, ny) | (x, y) <- region, (nx, ny) <- neighbors x y]
    isExposed grid region (x, y, nx, ny)
      | nx < 0 || ny < 0 || nx >= length grid || ny >= length (head grid) = True
      | (grid !! nx !! ny) /= (grid !! x !! y) = True
      | (nx, ny) `notElem` region = True
      | otherwise = False

-- Example usage
main :: IO ()
main = do
  -- Specify the path to your input file
  let filePath = "day12/input.txt"  -- replace this with your actual file path
  content <- readFile filePath
  let grid = parseMap content
  totalCost <- calculateTotalCost grid
  print totalCost
