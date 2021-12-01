import Data.List

readInts :: FilePath -> IO[Int]
readInts file = map toInt . lines <$> readFile file
  where
    toInt :: String -> Int
    toInt = read

windows :: Int -> [a] -> [[a]]
windows n = filter ((== n) . length) . map (take n) . tails

ex1 :: [Int] -> Int
ex1 []     = 0
ex1 (h:t) = go t h 0
  where
    go [] _ a   = a
    go (x:xs) y a
      | x > y     = go xs x a + 1
      | otherwise = go xs x a

ex2 :: [Int] -> Int
ex2 l = ex1 . map sum $ windows 3 l

main :: IO ()
main = do
  input <- readInts "input"
  let p1 = ex1 input
  let p2 = ex2 input
  print p1
  print p2
