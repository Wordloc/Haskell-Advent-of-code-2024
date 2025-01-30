import Data.List (transpose, isPrefixOf)

-- Count occurrences of a substring, handling overlaps
countOccurrences :: String -> String -> Int
countOccurrences needle haystack
    | length haystack < length needle = 0
    | needle `isPrefixOf` haystack = 1 + countOccurrences needle (tail haystack)
    | otherwise = countOccurrences needle (tail haystack)

-- Helper function to count occurrences in rows and columns
wordCountHelper :: String -> String -> Int
wordCountHelper line word = countOccurrences word line + countOccurrences (reverse word) line

-- Row search: search each row for the word and its reverse
rowSearch :: [String] -> String -> Int
rowSearch grid word = sum $ map (`wordCountHelper` word) grid

-- Column search: search each column for the word and its reverse
colSearch :: [String] -> String -> Int
colSearch grid word = sum $ map (`wordCountHelper` word) (transpose grid)

-- Diagonal search: find the diagonals and search them for the word and its reverse
diagSearch :: [String] -> String -> Int
diagSearch grid word = sum (map (`wordCountHelper` word) allDiags)
  where
    -- Get all diagonals (both directions)
    allDiags = getDiagonals grid ++ getDiagonals (map reverse grid)

-- Get diagonals from top-left to bottom-right and top-right to bottom-left
getDiagonals :: [String] -> [String]
getDiagonals grid = map (collectDiagonal grid) [0..(length (head grid) + length grid - 2)]
  where
    collectDiagonal :: [String] -> Int -> String
    collectDiagonal g i = [g !! r !! c | r <- [0..length g - 1], let c = i - r, c >= 0, c < length (g !! r)]

-- Main function
main :: IO ()
main = do
    -- Read the input file and split it into lines
    content <- lines <$> readFile "day04/day4_puzzle_input.txt"
    
    -- Count matches in rows, columns, and diagonals
    let word = "XMAS"
        rowMatches = rowSearch content word
        colMatches = colSearch content word
        diagMatches = diagSearch content word
        
    -- Output the results
    putStrLn $ "Row Matches: " ++ show rowMatches
    putStrLn $ "Col Matches: " ++ show colMatches
    putStrLn $ "Diag Matches: " ++ show diagMatches
    putStrLn $ "Part 1 Matches: " ++ show (rowMatches + colMatches + diagMatches)
