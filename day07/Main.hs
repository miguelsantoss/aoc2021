{-# LANGUAGE OverloadedStrings #-}

import Data.List (foldl')
import qualified Data.Text as T
import qualified Data.Text.Read as TR

toInt :: T.Text -> Int
toInt = fst . either error id . TR.decimal

parse :: String -> [Int]
parse = map toInt . T.splitOn "," . T.pack

align :: [Int] -> Int -> Int
align xs n = foldl' (\acc x -> acc + abs (x - n)) 0 xs

align2 :: [Int] -> Int -> Int
align2 xs n = foldl' sumF 0 xs
  where
    sumF acc x =
      let diff = abs (x - n)
          cost = (diff * (diff + 1)) `div` 2
       in acc + cost

solve1 :: [Int] -> Int
solve1 xs = minimum $ map (align xs) [min .. max]
  where
    min = minimum xs
    max = maximum xs

solve2 :: [Int] -> Int
solve2 xs = minimum $ map (align2 xs) [min .. max]
  where
    min = minimum xs
    max = maximum xs

main :: IO ()
main = do
  input <- head . lines <$> readFile "input"
  let parsed = parse input
      s1 = solve1 parsed
      s2 = solve2 parsed
  print s1
  print s2
