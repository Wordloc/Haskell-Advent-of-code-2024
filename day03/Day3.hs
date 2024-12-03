import System.IO (readFile)
import Text.Regex.TDFA ((=~))
import Data.Maybe (mapMaybe)

-- Parse the input text and find all substrings matching the pattern mul(X,Y)
parseMultiples :: String -> [(Int, Int)]
parseMultiples text = mapMaybe extractNumbers matches
  where
    -- Regex to match mul(X,Y) where X and Y are 1-3 digit numbers
    regex = "mul\\(([0-9]{1,3}),([0-9]{1,3})\\)" :: String
    -- Find all matches
    matches = text =~ regex :: [[String]]
    -- Extract X and Y from each match as a tuple of integers
    extractNumbers :: [String] -> Maybe (Int, Int)
    extractNumbers (_ : x : y : _) = Just (read x, read y)
    extractNumbers _              = Nothing

-- Multiply pairs and sum up all results
sumOfMultiples :: [(Int, Int)] -> Int
sumOfMultiples pairs = sum [x * y | (x, y) <- pairs]

-- Main function
main :: IO ()
main = do
    content <- readFile "day3_puzzle_input.txt" -- Read the input text
    let pairs = parseMultiples content
        total = sumOfMultiples pairs
    print total -- Print the total sum of all valid multiples

