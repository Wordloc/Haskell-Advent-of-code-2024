import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Char (digitToInt)

-- Type aliases for clarity
type Coord = (Int, Int)
type Grid = Map.Map Coord Int

-- Shared utility to find neighbors of a coordinate
neighbors :: Coord -> [Coord]
neighbors (x, y) = [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]

-- Part 1: Calculate the sum of the sizes of all trails
part1 :: Grid -> Int
part1 grid = sum . map (Set.size . exploreToHeight 0 Set.empty) . Map.keys . Map.filter (== 0) $ grid
  where
    exploreToHeight :: Int -> Set.Set Coord -> Coord -> Set.Set Coord
    exploreToHeight currentHeight visitedCoords coord
      | currentHeight == 9 = Set.insert coord visitedCoords
      | otherwise =
          Set.unions
            [ exploreToHeight neighborHeight visitedCoords neighbor
              | neighbor <- neighbors coord,
                Just neighborHeight <- [Map.lookup neighbor grid],
                neighborHeight == currentHeight + 1
            ]

-- Part 2: Count the total number of valid paths to height 9
part2 :: Grid -> Int
part2 grid = 
  sum . map (countPathsToHeight 0) $ trailheads
  where
    trailheads = Map.keys $ Map.filter (== 0) grid

    countPathsToHeight :: Int -> Coord -> Int
    countPathsToHeight currentHeight coord
      | currentHeight == 9 = 1
      | otherwise =
          sum [ countPathsToHeight nextHeight neighbor
                | neighbor <- neighbors coord,
                  Just nextHeight <- [Map.lookup neighbor grid],
                  nextHeight == currentHeight + 1
              ]

-- Main function to parse the input and compute the results
main :: IO ()
main = do
  -- Read the input file as a list of lines
  rawInput <- lines <$> readFile "day10/input.txt"

  -- Parse the input into a Grid (a map of coordinates to heights)
  let grid = Map.fromList
        [ ((x, y), digitToInt char)
          | (y, row) <- zip [0 ..] rawInput,    -- y-coordinates for rows
            (x, char) <- zip [0 ..] row        -- x-coordinates for columns
        ]

  -- Compute and print results for Part 1 and Part 2
  putStrLn $ "Part 1: " ++ show (part1 grid)
  putStrLn $ "Part 2: " ++ show (part2 grid)

