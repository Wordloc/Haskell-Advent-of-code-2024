{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
import Data.Graph
import Data.List
import Data.List.Split (splitOn)
import Data.Array (listArray, (!))
import Data.Maybe
import Data.Function (on)
import Debug.Trace

type Page = Int
type Rule = (Page, Page)

----- Parsing Section -----

-- Parse a line into a Rule (x|y)
parseRule :: String -> Rule
parseRule line = 
    let [x, y] = map read (splitOn "|" line) -- Simply split on "|"
    in (x, y)

-- Parse a single update (e.g "17,14,76,84,39")
parseUpdate :: String -> [Page]
parseUpdate line = map read (splitOn "," line) -- Split by "," and read the integers

-- Read all the rules and updates from input lines 
parseInput :: String -> ([Rule], [[Page]])
parseInput input =
  let linesInput = lines input
      (rulesSection, updatesSection) = break null linesInput
      rules = [parseRule line | line <- rulesSection]
      -- Safely handle the case where updatesSection is empty
      updates = if null updatesSection then [] else map parseUpdate (tail updatesSection)  -- Only process updates if the list is non-empty
  in (rules, updates)


----- Graph construction -----

-- Add subgraph functionality
nodeInducedSubgraph :: [Page] -> [Rule] -> [Rule]
nodeInducedSubgraph subNodes rules =
    filter (\(x, y) -> x `elem` subNodes && y `elem` subNodes) rules

-- Build graph from rules 
buildGraph :: [Rule] -> Maybe [Page] -> (Graph, Vertex -> (Page, Page, [Page]), Page -> Maybe Vertex)
buildGraph rules maybeSubNodes =
  let subRules = case maybeSubNodes of
                    Just subNodes -> nodeInducedSubgraph subNodes rules
                    Nothing -> rules
      pages = nub $ sort $ [x | (x, _) <- subRules] ++ [y | (_, y) <- subRules]
      -- vertexMap = zip pages [0..]
      -- pageToVertex page = lookup page vertexMap
      edges = subRules
      connections = [(page, page, [y | (x, y) <- subRules, page == x]) | page <- pages]
  in graphFromEdges connections


-- Check if a graph contains cycles using scc 
hasCycles :: Graph -> Bool
hasCycles graph = any (\component -> length component > 1) (scc graph)

-- Validate an update and do the topSort
sortUpdate :: [Rule] -> [Page] -> [Page]
sortUpdate rules update =
  let (graph, vtxToNode, pageToVtx) = buildGraph rules (Just update)
      updateVertices = map (fromJust .pageToVtx) update
      sorted = topSort graph
      vtxToPage vtx = let (page, _, _) = vtxToNode vtx
                      in page
  in map vtxToPage sorted

-- Compute the middle page
middlePage :: [Page] -> Page
middlePage update = update !! (length update `div` 2)


-- Process update with cycle detection
processUpdate :: [Rule] -> [Page] -> Int
processUpdate rules update =
  let sorted = sortUpdate rules update
  in  if sorted /= update
      then middlePage sorted
      else 0


-- Main function to integrate parsing and processing
main :: IO ()
main = do
    -- Read input from text files 
    input <-readFile "day05/day5_puzzle_input.txt"

    -- Parse the input into rules and updates
    let (rules, updates) = parseInput input

    -- Process the updates and print the result
    let result = sum $ map (processUpdate rules) updates
    print result
