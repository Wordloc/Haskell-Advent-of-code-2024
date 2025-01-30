import Control.Monad (replicateM)
import Data.List (subsequences)
import Text.Read (readMaybe)
import Debug.Trace (trace)
import Data.Maybe (mapMaybe)

-- Evaluate a list of numbers with a list of operators left-to-right
evaluate :: [Int] -> [Char] -> Int
evaluate (x:xs) (op:ops) = evaluate' x xs (op:ops)
  where
    evaluate' acc [] [] = acc
    evaluate' acc (y:ys) (o:os) = evaluate' (applyOp acc o y) ys os
    applyOp a '+' b = a + b
    applyOp a '*' b = a * b
    applyOp a '|' b = read (show a ++ show b) -- Concatenate numbers as strings and convert to integer

-- Generate all possible operator combinations for a given list of numbers
generateOperatorCombinations :: Int -> [[Char]]
generateOperatorCombinations n = replicateM (n-1) ['+', '*', '|'] -- Added '|' as a possible operator

-- Check if an equation is valid, i.e., if it can be formed to match the target value
isValidEquation :: Int -> [Int] -> Bool
isValidEquation targetValue nums = any (== targetValue) evaluated
  where
    -- Generate all operator combinations
    operatorCombinations = generateOperatorCombinations (length nums)
    -- Evaluate the expression for each operator combination
    evaluated = map (uncurry evaluate) [(nums, ops) | ops <- operatorCombinations]

-- Parse the input and check each equation
parseInput :: String -> [(Int, [Int])]
parseInput input = mapMaybe parseLine (lines input)
  where
    parseLine line = trace ("Parsing line: " ++ line) $
        let (targetPart, rest) = break (== ':') line
            targetStr = filter (/= ' ') targetPart -- Remove spaces
            numbersStr = drop 1 rest               -- Skip the colon
        in case readMaybe targetStr of
            Just target -> 
                let numbers = mapMaybe readMaybe (words numbersStr)
                in trace ("Parsed: " ++ show (target, numbers)) $ Just (target, numbers)
            Nothing -> trace ("Invalid target: " ++ targetStr) Nothing

-- Solve the problem for part 2
solvePart2 :: String -> Int
solvePart2 input = sum [target | (target, numbers) <- parsedInput, isValidEquation target numbers]
  where
    parsedInput = parseInput input

-- Main function to read the input from a file and solve the problem
main :: IO ()
main = do
    -- Read the content of the file into a string
    content <- readFile "day07/input.txt"
    
    -- Print the parsed content to ensure it's being read properly
    trace ("Parsed Input: " ++ show (parseInput content)) (return ())

    -- Solve the problem for part 1 and part 2 using the input data
    -- let resultPart1 = solvePart1 content
    let resultPart2 = solvePart2 content
    
    -- Print the result as a tuple (part1, part2)
    trace ("Final Result: " ++ show resultPart2) (return ())
    print resultPart2

