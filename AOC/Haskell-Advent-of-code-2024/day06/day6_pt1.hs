import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

-- Set the directions as deltas 
data Direction = N | E | S | W deriving (Show, Enum)

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
    let coordinates = [((r, c), char) | (r, row) <- zip[0..] rows, (c, char) <- zip[0..] row]
        grid = Map.fromList coordinates
        (startPos, dirChar) = head [(pos, char) | (pos, char) <- coordinates, char `elem` "^>v<"]
        startDir = case dirChar of 
            '^' -> N
            '>' -> E
            'v' -> S
            '<' -> W
    in (grid, startPos, startDir)

-- Simulating the guard's movements
simulate :: Map (Int, Int) Char -> (Int, Int) -> Direction -> Int
simulate grid startPos startDir = 
    let
        -- Recursive simulation function 
        simulate' :: (Int, Int) -> Direction -> Set (Int, Int) -> Set (Int, Int)
        simulate' pos dir visited
            | not (Map.member pos grid) = visited --  Guard out of bounds
            | otherwise =
                let (dr, dc) = delta dir
                    nextPos = (fst pos + dr, snd pos + dc)
                in case Map.lookup nextPos grid of
                    Just '#' -> simulate' pos (turnRight dir) visited -- Simulates turning right at an obstacle
                    Just _ -> simulate' nextPos dir (Set.insert nextPos visited) -- Simulates moving forward
                    Nothing -> visited -- Guard is out of bounds at nextPos 
    in Set.size (simulate' startPos startDir(Set.singleton startPos))

-- Main function to integrate parsing and processing
main :: IO ()
main = do
    content <- readFile "day06/input.txt"
    let input = lines content
    let (grid, startPos, startDir) = parseInput input
    print $ simulate grid startPos startDir
