{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.List
import qualified Data.Text as T
import Debug.Trace

type Input = ([T.Text], [T.Text])

fromJust :: Maybe a -> a
fromJust (Just a) = a
fromJust Nothing = error "oops"

easySegments :: [Int]
easySegments = [2, 3, 4, 7]

parseLine :: [T.Text] -> Input
parseLine [sig, outp] = (signals, out)
  where
    split = T.splitOn " "
    signals = split sig
    out = split outp
parseLine _ = error ""

parse :: [String] -> [Input]
parse = map (parseLine . T.splitOn " | " . T.pack)

countEasyNumbers :: [T.Text] -> Int
countEasyNumbers = length . filter (`elem` easySegments) . map T.length

solve1 :: [Input] -> Int
solve1 xs = count
  where
    count = foldl' (\acc (_, x) -> acc + countEasyNumbers x) 0 xs

solveLetters :: Input -> Int
solveLetters (xs, ys) = fromDigits numbers
  where
    strings = map T.unpack (xs ++ ys)
    !n1 = fromJust $ find (\x -> length x == 2) strings
    !n4 = fromJust $ find (\x -> length x == 4) strings
    !n7 = fromJust $ find (\x -> length x == 3) strings
    !n8 = fromJust $ find (\x -> length x == 7) strings
    !bd = filter (\x -> x `notElem` n1 ++ n7) n4
    !n0or9 = filter (\x -> length x == 6) strings
    !n0or9u = nub $ foldl' (++) [] n0or9
    !notcde = foldl' intersect n0or9u n0or9
    !cde = filter (`notElem` notcde) n8
    !a = fromJust $ find (\x -> x `notElem` n1 ++ n4) n7
    !c = fromJust $ find (\x -> x `elem` n1 && x `elem` n4 && x `elem` n7 && x `elem` n8) cde
    !d = fromJust $ find (\x -> x `notElem` n1 && x `elem` n4 && x `notElem` n7 && x `elem` n8) cde
    !e = fromJust $ find (\x -> x `notElem` n1 && x `notElem` n4 && x `notElem` n7 && x `elem` n8) cde
    !b = fromJust $ find (/= d) bd
    !f = fromJust $ find (\x -> x `elem` n1 && x `elem` n4 && x `elem` n7 && x `elem` n8 && x /= c) n8
    !g = fromJust $ find (\x -> x `notElem` n1 && x `notElem` n4 && x `notElem` n7 && x `elem` n8 && x /= e) n8

    getNumber xs
      | length xs == 6 && d `notElem` xs = 0
      | length xs == 2 = 1
      | length xs == 5 && b `notElem` xs && f `notElem` xs = 2
      | length xs == 5 && b `notElem` xs && e `notElem` xs = 3
      | length xs == 4 = 4
      | length xs == 5 && c `notElem` xs && e `notElem` xs = 5
      | length xs == 6 && c `notElem` xs = 6
      | length xs == 3 = 7
      | length xs == 7 = 8
      | length xs == 6 && e `notElem` xs = 9
    numbers = map (getNumber . T.unpack) ys

fromDigits :: [Int] -> Int
fromDigits = foldl' addDigit 0
  where
    addDigit num d = 10 * num + d

solve2 :: [Input] -> Int
solve2 xs = sum $ map solveLetters xs

main :: IO ()
main = do
  input <- lines <$> readFile "input"
  let parsed = parse input
      s1 = solve1 parsed
      s2 = solve2 parsed

  print s2