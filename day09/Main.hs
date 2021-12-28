{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Char (digitToInt)
import Data.List (foldl', nub, sortBy)

parse :: String -> [[Int]]
parse = map (map digitToInt) . lines

lowest :: [[Int]] -> [[(Int, Int, Int)]]
lowest xs = goAll 0 0 []
  where
    goAll x y points
      | x == maxX && y == maxY = newP
      | x == maxX = goAll 0 (y + 1) newP
      | otherwise = goAll (x + 1) y newP
      where
        !res = checkLowest x y
        newP = case res of
          Just n -> getBasins x y n points [] : points
          Nothing -> points

    !maxX = length (head xs) - 1
    !maxY = length xs - 1

    getBasins :: Int -> Int -> Int -> [[(Int, Int, Int)]] -> [(Int, Int)] -> [(Int, Int, Int)]
    getBasins x y curr existing visited = nub newPositions
      where
        !n = xs !! y !! x
        !isValid = n >= curr && n < 9 && not (any (elem (x, y, n)) existing)
        !around = if isValid then [(x - 1, y), (x, y - 1), (x + 1, y), (x, y + 1)] else []
        !valid = filter (\(x, y) -> x >= 0 && y >= 0 && x <= maxX && y <= maxY && (x, y) `notElem` visited) around
        !rest = map (\(x2, y2) -> getBasins x2 y2 (n + 1) existing ((x, y) : visited)) valid
        !initial = [(x, y, n) | isValid]
        !newPositions = foldl' (++) initial rest

    checkLowest :: Int -> Int -> Maybe Int
    checkLowest x y
      | left && up && right && down = Just n
      | otherwise = Nothing
      where
        !n = xs !! y !! x
        left = (x == 0) || n < (xs !! y !! (x - 1))
        up = (y == 0) || n < (xs !! (y - 1) !! x)
        down = (y == maxY) || n < (xs !! (y + 1) !! x)
        right = (x == maxX) || n < (xs !! y !! (x + 1))

solve1 :: [[(Int, Int, Int)]] -> Int
solve1 = sum . map ((+ 1) . minimum . map third)

solve2 :: [[(Int, Int, Int)]] -> Int
solve2 = product . take 3 . sortBy (flip compare) . map length

third :: (a, b, c) -> c
third (x, y, z) = z

main :: IO ()
main = do
  input <- readFile "input"
  let parsed = parse input
      basins = lowest parsed
      s1 = solve1 basins
      s2 = solve2 basins
  print s1
  print s2
