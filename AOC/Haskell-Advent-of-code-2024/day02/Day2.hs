import System.IO(readFile)

-- Parse a single line into a list of intergers
parseLine :: String -> [Int]
parseLine line = map read (words line)

-- Check if a list is strictly increasing 
isIncreasing :: [Int] -> Bool
isIncreasing xs = all (\(a, b) -> b > a) (zip xs (tail xs))

-- Check if a list is strictly decreasing 
isDecreasing :: [Int] -> Bool
isDecreasing xs = all (\(a, b) -> b < a) (zip xs (tail xs))

-- Check if all differences are between 1 and 3 and thus "safe"
safeDifferences :: [Int] -> Bool
safeDifferences xs = all (\(a, b) -> abs (b - a) >= 1 && abs (b - a) <= 3) (zip xs (tail xs))

-- Check if list satisfies both original conditions
isValidList :: [Int] -> Bool 
isValidList xs = (isIncreasing xs || isDecreasing xs) && safeDifferences xs

-- Generate all sublists of a list by removing one element
sublistsByRemoval :: [a] -> [[a]]
sublistsByRemoval xs = [take i xs ++ drop (i + 1) xs | i <- [0 .. length xs - 1]]

-- Check if a list is "safe" or can be made "safe" by the problem dampener (removing one element)
isSafeList :: [Int] -> Bool
isSafeList xs = isValidList xs || any isValidList (sublistsByRemoval xs)

-- Part 1: Count of "safe" lists w/o removing an element
part1 :: [[Int]] -> Int
part1 lists = length (filter isValidList lists)

-- Part 2: Count of "safe" lists w/ removal of one element
part2 :: [[Int]] -> Int
part2 lists = length (filter isSafeList lists)

-- Main function to read puzzle input and count valid lines 
main :: IO ()
main = do
    content <- readFile "day02/day2_puzzle_input.txt" -- reads in puzzle input
    let lists = map parseLine (lines content) -- parse each line
        part1Result = part1 lists -- calculates part 1 result
        part2Result = part2 lists -- calculates part 2 result
    print (part1Result, part2Result) -- print both results as a tuple