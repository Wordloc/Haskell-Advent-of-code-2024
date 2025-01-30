import Data.Map.Strict (Map)  -- Use strict map for better performance
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Debug.Trace

-- Set the directions as deltas 
data Direction = N | E | S | W deriving (Show, Enum, Ord, Eq)

-- Turn right instruction (expected behaviour when reaching an obstacle)
turnRight :: Direction -> Direction
turnRight dir = case dir of
    N -> E
    E -> S
    S -> W
    W -> N

-- Set up movement deltas for each direction
delta :: Direction -> (Int, Int)
delta N = (-1, 0) -- Move Up
delta E = (0, 1) -- Move Right
delta S = (1, 0) -- Move Down
delta W = (0, -1) -- Move Left

-- Parse input into a map of coordinates 
parseInput :: [String] -> (Map (Int, Int) Char, (Int, Int), Direction)
parseInput rows =
    let coordinates = [((r, c), char) | (r, row) <- zip [0..] rows, (c, char) <- zip [0..] row]
        grid = Map.fromList coordinates
        (startPos, dirChar) = head [(pos, char) | (pos, char) <- coordinates, char `elem` "^>v<"]
        startDir = case dirChar of
            '^' -> N
            '>' -> E
            'v' -> S
            '<' -> W
    in (grid, startPos, startDir)

-- Function to check if a position is a valid place to add an obstruction
validPositions :: Map (Int, Int) Char -> (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
validPositions _grid startPos visited = filter (/= startPos) visited

-- Simulating the guard's movements -- Modified to accept an added obstruction at a specific position
simulateWithObstruction :: Map (Int, Int) Char -> (Int, Int) -> Direction -> (Int, Int) -> Bool
simulateWithObstruction grid startPos startDir obstruction =
    let -- Add the obstruction temporarily
        gridWithObstruction = Map.insert obstruction '#' grid

        -- Recursive simulation function 
        simulate' :: (Int, Int) -> Direction -> Set ((Int, Int), Direction) -> Bool
        simulate' pos dir visited
            | (pos,dir) `Set.member` visited = traceShow (obstruction,Set.size visited) True
            | otherwise =
                let (dr, dc) = delta dir
                    nextPos = (fst pos + dr, snd pos + dc)
                in case Map.lookup nextPos gridWithObstruction of
                    Just '#' -> simulate' pos (turnRight dir) visited -- Simulates turning right at an obstacle
                    Just _ -> simulate' nextPos dir (Set.insert (pos,dir) visited) -- Simulates moving forward
                    Nothing -> False -- Guard is out of bounds at nextPos 
    in simulate' startPos startDir Set.empty

-- Function to find all positions that cause the guard to get stuck in a loop 
findLoopPositions :: Map (Int, Int) Char -> (Int, Int) -> Direction -> Int
findLoopPositions grid startPos startDir =
    let validPositionsList = validPositions grid startPos (Map.keys grid)
    in length [pos | pos <- validPositionsList, simulateWithObstruction grid startPos startDir pos]

-- Main function to integrate parsing and processing
main :: IO ()
main = do
    content <- readFile "day06/input.txt"
    let input = lines content
    let (grid, startPos, startDir) = parseInput input

    -- Part 2: Find the number of positions that cause the guard to get stuck in a loop 
    let part2Answer = findLoopPositions grid startPos startDir
    print part2Answer -- 3min 20s

