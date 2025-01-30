import Data.List (transpose)

-- XMAS Cross search: look for the pattern where 'A' is surrounded by 'S' and 'M'
xmasCrossSearch :: [String] -> Int
xmasCrossSearch grid = length $ filter isXmasCross [((r, c)) | r <- [1..(length grid-2)], c <- [1..(length (head grid) - 2)]] 
  where
    isXmasCross (r, c) =
      let topLeft = (grid !! (r-1)) !! (c-1)
          topRight = (grid !! (r-1)) !! (c+1)
          bottomLeft = (grid !! (r+1)) !! (c-1)
          bottomRight = (grid !! (r+1)) !! (c+1)
          countS = length (filter (== 'S') [topLeft, topRight, bottomLeft, bottomRight])
          countM = length (filter (== 'M') [topLeft, topRight, bottomLeft, bottomRight])
      in grid !! r !! c == 'A' && countS == 2 && countM == 2 && 
         not ((topLeft == 'S' && bottomRight == 'S') || (topRight == 'S' && bottomLeft == 'S'))

-- Main function
main :: IO ()
main = do
    -- Read the input file and split it into lines
    content <- lines <$> readFile "day04/day4_puzzle_input.txt"
    
    -- Count matches in X-MAS cross pattern
    let xmasCrossMatches = xmasCrossSearch content
        
    -- Output the results
    putStrLn $ "X-MAS Cross Matches: " ++ show xmasCrossMatches
