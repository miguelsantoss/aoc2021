{-# LANGUAGE OverloadedStrings #-}

import Data.List
import qualified Data.Char as C
import qualified Data.Text as T
import qualified Data.Text.Read as TR

type Board = [[Int]]

toInt :: T.Text -> Int
toInt = fst. either error id . TR.decimal

chunks :: Int -> [a] -> [[a]]
chunks n [] = []
chunks n xs = head : (chunks n tail)
  where (head, tail) = splitAt n xs
  
parseBoard :: [T.Text] -> Board
parseBoard = filter (not . null) . map parseLine
  where 
    parseLine = map toInt . filter (not . T.null) . (T.splitOn " ") . T.strip

parse :: [String] -> ([Int], [Board])
parse l = (numbers, boards)
  where 
    numbers = (map toInt) . (T.splitOn ",") . T.pack $ head l
    boards = map parseBoard . chunks 6 . map T.pack . drop 2 $ l

mark :: Int -> Board -> Board
mark _ []     = []
mark n (x:xs) = map (markN n) x : mark n xs
  where 
    markN n x
      | n == x    = -1
      | otherwise = x

sumBoard :: Board -> Int
sumBoard xs = sum . map sum $ rest
  where rest = map (filter (>0)) xs

hasWinner :: Board -> Bool
hasWinner xs = checkRow xs || (checkRow . transpose) xs
  where checkRow = any (all (<0))

winner1 :: [Int] -> [Board] -> (Int, Board)
winner1 (n:ns) boards = case winner of
  Just wb -> (n, wb)
  Nothing  -> winner1 ns filtered
  where
    filtered = map (mark n) boards
    winner = find hasWinner filtered

solve1 :: [Int] -> [Board] -> Int
solve1 ns boards = last * (sumBoard winnerB)
  where 
    (last, winnerB) = winner1 ns boards

winner2 :: [Int] -> [Board] -> (Int, Board)
winner2 (n:ns) boards = case losing of 
  [] -> (n, (head filtered))
  xs -> winner2 ns xs
  where
    filtered = map (mark n) boards
    losing = filter (not . hasWinner) filtered

solve2 :: [Int] -> [Board] -> Int
solve2 ns boards = last * (sumBoard winnerB)
  where 
    (last, winnerB) = winner2 ns boards

main :: IO ()
main = do
  input <- lines <$> readFile "input"
  let (numbers, boards) = parse input
  let s1 = solve1 numbers boards
      s2 = solve2 numbers boards
  print s1
  print s2