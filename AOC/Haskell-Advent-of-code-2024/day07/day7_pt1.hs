import Control.Monad (replicateM)
import Data.List (subsequences)
import Text.Read (readMaybe)
import Data.Maybe (mapMaybe)
import Debug.Trace (trace)

-- Evaluate a list of numbers with a list of operators left-to-right
evaluate :: [Int] -> [Char] -> Int
evaluate (x:xs) (op:ops) = evaluate' x xs (op:ops)
  where
    evaluate' acc [] [] = acc
    evaluate' acc (y:ys) (o:os) = evaluate' (applyOp acc o y) ys os
    applyOp a '+' b = a + b
    applyOp a '*' b = a * b

-- Generate all possible operator combinations for a given list of numbers
generateOperatorCombinations :: Int -> [[Char]]
generateOperatorCombinations n = replicateM (n-1) ['+', '*']

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

-- Solve the problem
solve :: String -> Int
solve input = sum [target | (target, numbers) <- parsedInput, isValidEquation target numbers]
  where
    parsedInput = parseInput input

-- Main function to read the input from a file and solve the problem
main :: IO ()
main = do
    -- Read the content of the file into a string
    content <- readFile "day07/input.txt"
    
    -- Solve the problem using the input data
    let result = solve content
    
    -- Print the result
    print result
