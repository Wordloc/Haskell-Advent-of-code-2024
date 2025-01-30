import Data.Char
import Data.List hiding (group)
import Data.Sequence (Seq ((:<|)), (><))
import Data.Sequence qualified as Seq
import Data.Foldable (toList) -- Fix for `toList`

type Parsed = [Int]

parseInput :: String -> Parsed
parseInput = map digitToInt . init

data Block = FileBlock FileId | EmptyBlock deriving (Show, Eq)

type FileId = Int

decode :: [Int] -> [(Block, Int)]
decode = zip (intersperse EmptyBlock (map FileBlock [0 ..]))

unRunLen :: [(a, Int)] -> [a]
unRunLen = concatMap (uncurry (flip replicate))

compact :: [Block] -> [FileId]
compact blocks =
  let numFileBlocks = length (filter (/= EmptyBlock) blocks)
   in take numFileBlocks (go blocks (reverse blocks))
  where
    go [] _ = []
    go _ [] = []
    go (FileBlock fileId : xs) ys = fileId : go xs ys
    go (EmptyBlock : xs) (FileBlock fileId : ys) = fileId : go xs ys
    go xs@(EmptyBlock : _) (EmptyBlock : ys) = go xs ys

compact2 :: [(Block, Int)] -> [(Block, Int)]
compact2 blocks = toList . foldl' go (Seq.fromList blocks) . reverse $ blocks
  where
    go xs y@(FileBlock fileId, size) =
      let (ps, _ :<| ss) = Seq.breakl (== y) xs
       in case Seq.breakl isSufficientlyLargeEmptyBlock ps of
            (pps, (_, size') :<| sps) ->
              pps
                >< ((FileBlock fileId, size) :<| (EmptyBlock, size' - size) :<| sps)
                >< ((EmptyBlock, size) :<| ss)
            (_, Seq.Empty) -> xs
      where
        isSufficientlyLargeEmptyBlock (EmptyBlock, size') = size' >= size
        isSufficientlyLargeEmptyBlock _ = False
    go xs (EmptyBlock, _) = xs

checksum :: [FileId] -> Int
checksum = sum . zipWith (*) [0 ..]

solve1 :: Parsed -> Int
solve1 = checksum . compact . unRunLen . decode

checksum2 :: [Block] -> Int
checksum2 =
  sum
    . zipWith
      ( \mult block -> case block of
          FileBlock fileId -> mult * fileId
          EmptyBlock -> 0
      )
      [0 ..]

solve2 :: Parsed -> Int
solve2 = checksum2 . unRunLen . compact2 . decode

instance {-# OVERLAPPING #-} Show [Block] where
  show =
    map (\x -> case x of
                 EmptyBlock -> '.'
                 FileBlock fileId -> intToDigit fileId)

-- Main function: Reads the input from the file, parses it, and calculates both checksums
main :: IO ()
main = do
  -- Read the input from the file "input.txt"
  input <- readFile "day09/input.txt"

  -- Parse the input into the Parsed format (a list of integers)
  let parsedInput = parseInput input

  -- Compute the solution for Part 1
  let part1Result = solve1 parsedInput

  -- Compute the solution for Part 2
  let part2Result = solve2 parsedInput

  -- Output the results as a tuple
  print (part1Result, part2Result)


-- (6360094256423,15753537894779)l
