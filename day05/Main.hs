{-# LANGUAGE OverloadedStrings #-}

import Data.Map (fromListWith, toList)
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import qualified System.Console.Terminfo.Base as T

type Point = (Int, Int)

type Inst = (Point, Point)

toInt :: T.Text -> Int
toInt = fst . either error id . TR.decimal

create :: [a] -> (a, a)
create (x : y : []) = (x, y)
create _ = error "missing stuff"

parsePoint :: T.Text -> Point
parsePoint = create . map toInt . T.splitOn ","

parseLine :: T.Text -> Inst
parseLine = create . map parsePoint . T.splitOn " -> "

parse :: [String] -> [Inst]
parse = map (parseLine . T.pack)

filterS1 :: Inst -> Bool
filterS1 ((x1, y1), (x2, y2)) = x1 == x2 || y1 == y2

generate1 :: Int -> Int -> [Int]
generate1 a b = [m1 .. m2]
  where
    m1 = min a b
    m2 = max a b

generateFromSlope :: Point -> Point -> Int -> Point
generateFromSlope (x1, y1) (x2, y2) x = (x, (m * (x - x2)) + y2)
  where
    m = yChange `div` xChange
    yChange = fromIntegral (y2 - y1)
    xChange = fromIntegral (x2 - x1)

generatePoints :: Inst -> [Point]
generatePoints ((x1, y1), (x2, y2))
  | x1 == x2 = map (\y -> (x1, y)) (generate1 y1 y2)
  | otherwise = map generate2 (generate1 x1 x2)
  where
    generate2 = generateFromSlope (x1, y1) (x2, y2)

frequency :: (Ord a) => [a] -> [(a, Int)]
frequency xs = toList (fromListWith (+) [(x, 1) | x <- xs])

solve1 :: [Inst] -> Int
solve1 xs = length $ filter atLeast2 freq
  where
    points = xs >>= generatePoints
    freq = frequency points
    atLeast2 (p, f) = f >= 2

main :: IO ()
main = do
  input <- lines <$> readFile "input"
  let parsed = parse input
      inst1 = filter filterS1 parsed
      s1 = solve1 inst1
      s2 = solve1 parsed

  print s1
  print s2