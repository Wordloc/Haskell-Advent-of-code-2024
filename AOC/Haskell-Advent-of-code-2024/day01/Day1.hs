import Data.List (sort)
import Data.List.Split (splitOn) -- import splitOn for custom delimeter "   "
import System.IO(readFile)
import qualified Data.Map as Map

-- Parsing lines from the puzzle input into a pair of integers
parseLine :: String -> (Int, Int)
parseLine line = let [a, b] = map read (splitOn "   " line) in (a, b)

-- Part 1: Compute the total distance (sum of absolute differences)
sumOfDistances :: [(Int, Int)] -> Int
sumOfDistances pairs = 
    let lefts = sort [x | (x, _) <- pairs]
        rights = sort [y | (_, y) <- pairs]
    in sum $ zipWith (\x y -> abs (x - y)) lefts rights

-- Part 2: Counting occurrences of each number in the RHS list
countOccurences :: [Int] -> Map.Map Int Int
countOccurences fst = Map.fromListWith (+) [(x, 1) | x <- fst]

-- Part 2: Calculating the "similarity score" (sum of lhs # * frequency in rhs)
calcSimilarityScore :: [Int] -> [Int] -> Int
calcSimilarityScore lefts rights = 
    let rightCounts = countOccurences rights
    in sum [x * Map.findWithDefault 0  x rightCounts | x <- lefts]

main :: IO ()
main = do
    -- Reads the file
    content <- readFile "day1_puzzle_input.txt"
    let pairs = map parseLine  (lines content)
        lefts  = [x | (x, _) <- pairs]
        rights = [y | (_, y) <- pairs]

        -- Part 1: calculate sumOfDistances
        part1Result = sumOfDistances pairs 
    
        -- Part 2: Calculate similarity score
        part2Result = calcSimilarityScore lefts rights

    -- Print results of parts 1 and 2 as a tuple
    print (part1Result, part2Result)