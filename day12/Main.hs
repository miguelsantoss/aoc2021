{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Char as C
import Data.List (find, group, sort)
import qualified Data.Map as M
import qualified Data.Text as T

type Graph = M.Map T.Text [T.Text]

toTuple :: [T.Text] -> [(T.Text, [T.Text])]
toTuple [x, y] = [(x, [y]), (y, [x])]
toTuple _ = error ""

parse :: String -> Graph
parse = M.fromListWith (++) . concatMap (toTuple . T.splitOn "-" . T.pack) . lines

isLower :: T.Text -> Bool
isLower = C.isLower . T.head

canVisitMultipleTimes :: T.Text -> Bool
canVisitMultipleTimes = not . isLower

canVisit1 :: [T.Text] -> T.Text -> Bool
canVisit1 path lk = canVisitMultipleTimes lk || lk `notElem` path

canVisit2 :: [T.Text] -> T.Text -> Bool
canVisit2 path lk
  | isLower lk = "start" /= lk && isAllowed
  | otherwise = True
  where
    allLower = group . sort . filter isLower $ path
    maxLowerVisits = find (\x -> length x > 1) allLower
    isAllowed = case maxLowerVisits of
      Just n -> lk `notElem` path
      Nothing -> True

findPaths :: ([T.Text] -> T.Text -> Bool) -> Graph -> [[T.Text]]
findPaths filterF g = go g ["start"] [] []
  where
    go :: Graph -> [T.Text] -> [[T.Text]] -> [T.Text] -> [[T.Text]]
    go g [] paths curr = paths
    go g ("end" : q) paths curr = ("end" : curr) : paths
    go g (h : q) paths curr = go g q (paths ++ newPaths) curr
      where
        !links = M.findWithDefault [] h g
        !toVisit = filter (filterF (h : curr)) links
        newPaths = concatMap (\l -> go g [l] paths (h : curr)) toVisit

solve1 :: Graph -> Int
solve1 = length . findPaths canVisit1

solve2 :: Graph -> Int
solve2 = length . findPaths canVisit2

main :: IO ()
main = do
  input <- readFile "input"
  let parsed = parse input
  print (solve1 parsed)
  print (solve2 parsed)
