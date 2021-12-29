{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Char (digitToInt)
import Data.List (foldl', nub, sortBy)

parse :: String -> [[Int]]
parse = map (map digitToInt) . lines

update1 :: (Int -> Int) -> Int -> [Int] -> [Int]
update1 _ _ [] = error "err"
update1 valf 0 (x : xs) = valf x : xs
update1 valf idx (x : xs) = x : update1 valf (idx - 1) xs

update2 :: (Int -> Int) -> Int -> Int -> [[Int]] -> [[Int]]
update2 _ _ _ [] = error "err"
update2 valf x1 0 (h : xs) = update1 valf x1 h : xs
update2 valf x1 y (h : xs) = h : update2 valf x1 (y -1) xs

neighbours :: Int -> Int -> [(Int, Int)]
neighbours x y = [(x - 1, y), (x -1, y -1), (x, y - 1), (x + 1, y -1), (x + 1, y), (x + 1, y + 1), (x, y + 1), (x -1, y + 1)]

flashAll :: [[Int]] -> [[Int]]
flashAll xs = updatedMatrix
  where
    !maxX = length (head xs) - 1
    !maxY = length xs - 1
    !toFlash = coordinatesToFlash xs
    !updatedMatrix = go xs toFlash

    go xs [] = xs
    go xs ((x, y) : q) = go (makeChanges valid newXs) (q ++ valid)
      where
        !flashed = xs !! y !! x > 9
        !newXs = if flashed then update2 (const 0) x y xs else xs
        valid = if flashed then filter inBounds (neighbours x y) else []
        inBounds (x, y) = x >= 0 && y >= 0 && x <= maxX && y <= maxY

makeChanges :: [(Int, Int)] -> [[Int]] -> [[Int]]
makeChanges [] hs = hs
makeChanges ((x, y) : xs) hs = makeChanges xs (update2 updateF x y hs)
  where
    updateF curr = if curr == 0 then 0 else curr + 1

coordinatesToFlash :: [[Int]] -> [(Int, Int)]
coordinatesToFlash xs = go xs 0 0 []
  where
    !maxX = length (head xs) - 1
    !maxY = length xs - 1

    go xs x y q
      | x == maxX && y == maxY = newQ
      | x == maxX = go xs 0 (y + 1) newQ
      | otherwise = go xs (x + 1) y newQ
      where
        !n = xs !! y !! x
        newQ = if n > 9 then (x, y) : q else q

step :: [[Int]] -> [[Int]]
step = flashAll . map (map (+ 1))

getFlashes :: [[Int]] -> Int
getFlashes = sum . map (length . filter (== 0))

solve1 :: [[Int]] -> Int -> Int
solve1 xs n = sum $ map getFlashes all
  where
    all = take (n + 1) $ iterate step xs

solve2 :: [[Int]] -> Int
solve2 xs = length . takeWhile (not . isBigFlash) $ iterate step xs
  where
    !maxX = length (head xs)
    !maxY = length xs
    !maxXY = maxX * maxY
    isBigFlash xs = getFlashes xs == maxXY

main :: IO ()
main = do
  input <- readFile "input"
  let parsed = parse input
      s1 = solve1 parsed 100
      s2 = solve2 parsed
  print s1
  print s2
