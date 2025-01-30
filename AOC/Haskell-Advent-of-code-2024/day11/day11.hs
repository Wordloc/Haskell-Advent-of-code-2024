import qualified Data.Map as Map
import Data.Map (Map)
import Data.Char (digitToInt)

-- Rule 1: Replace 0 with 1
rule1 :: Int -> Maybe [Int]
rule1 n
    | n == 0    = Just [1]
    | otherwise = Nothing

-- Rule 2: Split number with an even number of digits
rule2 :: Int -> Maybe [Int]
rule2 n
    | even (length digits) = 
        let (left, right) = splitAt (length digits `div` 2) digits
        in Just [toNumber left, toNumber right]
    | otherwise = Nothing
  where
    digits = toDigits n
    toNumber = foldl (\acc x -> acc * 10 + x) 0

-- Rule 3: Multiply by 2024
rule3 :: Int -> Maybe [Int]
rule3 n = Just [n * 2024]

-- Apply rules in order of priority
applyRules :: Int -> [Int]
applyRules n = head $ [result | rule <- rules, Just result <- [rule n]]
  where
    rules = [rule1, rule2, rule3]

-- Convert a number to its list of digits
toDigits :: Int -> [Int]
toDigits = map digitToInt . show

-- Apply rules to all stones and count their transformations
transform :: Map Int Int -> Map Int Int
transform stones = Map.fromListWith (+) $ concatMap expand $ Map.toList stones
  where
    expand (stone, count) = [(newStone, count) | newStone <- applyRules stone]

-- Simulate multiple blinks using the Map representation
simulateBlinks :: Map Int Int -> Int -> Map Int Int
simulateBlinks stones 0 = stones
simulateBlinks stones n = simulateBlinks (transform stones) (n - 1)

-- Count the total number of stones
countStones :: Map Int Int -> Int
countStones = sum . Map.elems

-- Main function
main :: IO ()
main = do
    -- Read input from file
    input <- readFile "day11/input.txt"
    let stones = map read $ words input :: [Int]
    
    -- Convert to map of counts
    let initialCounts = Map.fromListWith (+) [(stone, 1) | stone <- stones]
    
    -- Simulate 75 blinks
    let finalCounts = simulateBlinks initialCounts 75
    
    -- Output the total number of stones
    print $ countStones finalCounts

-- 25 blinks = 194557
