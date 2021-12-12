{-# LANGUAGE OverloadedStrings #-}

import Data.List
import qualified Data.Text as T
import qualified Data.Text.Read as TR

toInt :: T.Text -> Int
toInt = fst . either error id . TR.decimal

update :: (Int -> Int) -> Int -> [Int] -> [Int]
update _ _ [] = error "err"
update valf 0 (x : xs) = valf x : xs
update valf idx (x : xs) = x : update valf (idx - 1) xs

initial :: [Int]
initial = replicate 9 0

frequency :: [Int] -> [Int]
frequency = foldl' (flip (update (+ 1))) initial

parse :: String -> [Int]
parse = frequency . map toInt . T.splitOn "," . T.pack

advance1Day' :: [Int] -> [Int]
advance1Day' [] = error "err"
advance1Day' (x : xs) = newState
  where
    s1 = xs ++ [x]
    newState = update (+ x) 6 s1

advanceDays :: [Int] -> Int -> [Int]
advanceDays xs 0 = xs
advanceDays xs days = advanceDays (advance1Day' xs) (days - 1)

solve :: [Int] -> Int -> Int
solve xs n = sum $ advanceDays xs n

main :: IO ()
main = do
  input <- head . lines <$> readFile "input"
  let parsed = parse input
      s1 = solve parsed 80
      s2 = solve parsed 256
  print s1
  print s2
