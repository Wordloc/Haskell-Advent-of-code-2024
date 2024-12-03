import System.IO (readFile)
import Text.Regex.TDFA ((=~))
import Data.Maybe (mapMaybe)

-- Parse the input text for instructions
parseInstructions :: String -> [(String, Maybe (Int, Int))]
parseInstructions text = map parseInstruction matches
  where
    -- Regex to match do(), don't(), or mul(X,Y)
    regex = "do\\(\\)|don't\\(\\)|mul\\(([0-9]{1,3}),([0-9]{1,3})\\)" :: String
    -- Find all matches
    matches = text =~ regex :: [[String]]
    -- Parse each match into an instruction or a pair
    parseInstruction :: [String] -> (String, Maybe (Int, Int))
    parseInstruction match
      | head match == "do()" = ("do", Nothing)
      | head match == "don't()" = ("don't", Nothing)
      | otherwise = case tail match of
          (x : y : _) -> ("mul", Just (read x, read y))
          _           -> ("unknown", Nothing)

-- Process instructions and calculate the sum
processInstructions :: [(String, Maybe (Int, Int))] -> Int
processInstructions = go True 0
  where
    -- Recursive helper function with current state and accumulator
    go _ acc [] = acc
    go enabled acc ((instr, Nothing) : rest)
      | instr == "do" = go True acc rest
      | instr == "don't" = go False acc rest
      | otherwise = go enabled acc rest
    go enabled acc ((_, Just (x, y)) : rest)
      | enabled = go enabled (acc + (x * y)) rest
      | otherwise = go enabled acc rest

-- Main function
main :: IO ()
main = do
    content <- readFile "day3_puzzle_input.txt" -- Read the input text
    let instructions = parseInstructions content
        total = processInstructions instructions
    print total -- Print the total sum of all valid multiples