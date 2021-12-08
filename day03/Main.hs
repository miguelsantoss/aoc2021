import Data.Bool(bool)
import Data.List

toInt :: Char -> Int
toInt '0' = 0
toInt '1' = 1

asBool :: Char -> Bool
asBool '0' = False
asBool '1' = True

divCeil :: Int -> Int -> Int
divCeil a b = fromIntegral (ceiling ((fromIntegral a) / (fromIntegral b)))

count1 :: [Char] -> Bool
count1 l = count >= half
  where
    half = divCeil (length l) 2
    count = foldl' (+) 0 (map toInt l)

bin2dec :: (Foldable f, Integral i) => f Bool -> i
bin2dec = foldl (\a -> (+) (2*a) . bool 0 1) 0 

solve1 :: [[Char]] -> Int
solve1 l = decG * decE
  where
    bitsG = map count1 $ transpose l
    bitsE = map not bitsG
    decG = bin2dec bitsG
    decE = bin2dec bitsE


count2 :: (Bool -> Bool) -> [[Char]] -> [Char]
count2 f ([]:_) = []
count2 f (x:[]) = x
count2 f l      = asChar : count2 f rest
  where
    rest = map tail (filter (\x -> (head x) == asChar) l)
    first = map head l
    mc = count1 first
    asChar = if (f mc) then '1' else '0'

solve2 :: [[Char]] -> Int
solve2 l = o2 * co2
  where 
    toNum = bin2dec . (map asBool)
    o2 =  toNum . count2 id $ l
    co2 = toNum . count2 not $ l

main :: IO ()
main = do
  input <- lines <$> readFile "input"
  let s1 = solve1 input
      s2 = solve2 input
  print s1
  print s2