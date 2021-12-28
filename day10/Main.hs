{-# LANGUAGE OverloadedStrings #-}

import Data.List (group, sort)

data Del = Open Char | Close Char

createDel :: Char -> Del
createDel '(' = Open '('
createDel '[' = Open '['
createDel '{' = Open '{'
createDel '<' = Open '<'
createDel ')' = Close ')'
createDel ']' = Close ']'
createDel '}' = Close '}'
createDel '>' = Close '>'
createDel _ = error ""

closeChar :: Del -> Char
closeChar (Open '(') = ')'
closeChar (Open '[') = ']'
closeChar (Open '{') = '}'
closeChar (Open '<') = '>'
closeChar _ = error ""

isNewChunk :: Char -> Bool
isNewChunk '(' = True
isNewChunk '[' = True
isNewChunk '{' = True
isNewChunk '<' = True
isNewChunk _ = False

errorPoints :: Char -> Int
errorPoints ')' = 3
errorPoints ']' = 57
errorPoints '}' = 1197
errorPoints '>' = 25137

completePoints :: Char -> Int
completePoints ')' = 1
completePoints ']' = 2
completePoints '}' = 3
completePoints '>' = 4

matchesDel :: Char -> [Del] -> Bool
matchesDel c [] = False
matchesDel c (d : xs)
  | c == closeChar d = True
  | otherwise = False

solve1 :: [String] -> Int
solve1 = sum . map sumList . group . sort . map parseLine
  where
    sumList xs = head xs * length xs

    parseLine :: String -> Int
    parseLine xs = go xs []
      where
        go [] stack = 0
        go (c : xs) stack
          | isNewChunk c = go xs (Open c : stack)
          | matchesDel c stack = go xs (drop 1 stack)
          | otherwise = errorPoints c

solve2 :: [String] -> Int
solve2 lines = head $ drop (length sorted `div` 2) sorted
  where
    sorted = sort . filter (> 0) . map parseLine $ lines

    sumList xs = head xs * length xs

    finishLine :: [Del] -> Int -> Int
    finishLine [] acc = acc
    finishLine (x : xs) acc = finishLine xs (completePoints (closeChar x) + (acc * 5))

    parseLine :: String -> Int
    parseLine xs = go xs []
      where
        go [] stack = finishLine stack 0
        go (c : xs) stack
          | isNewChunk c = go xs (Open c : stack)
          | matchesDel c stack = go xs (drop 1 stack)
          | otherwise = 0

main :: IO ()
main = do
  input <- lines <$> readFile "input"
  print (solve1 input)
  print (solve2 input)
