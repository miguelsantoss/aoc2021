import Data.Char
import qualified Data.Text as T

data Direction = Forward Int | Down Int | Up Int deriving (Show)

fromStr :: String -> Direction
fromStr (h:t)
  | h == 'f' = Forward amount
  | h == 'd' = Down amount
  | h == 'u' = Up amount
  where
    toInt :: String -> Int
    toInt = read
    amount = toInt (dropWhile (not . isNumber) t)
fromStr _ = error "cannot parse"

newPosition :: (Int,Int) -> Direction -> (Int,Int)
newPosition (x,y) (Forward x1) = (x + x1,y)
newPosition (x,y) (Up y1)      = (x,y - y1)
newPosition (x,y) (Down y1)    = (x,y + y1)

newPositionWithAim :: (Int,Int,Int) -> Direction -> (Int,Int,Int)
newPositionWithAim (x,y,a) (Forward x1) = (x + x1, y + (x1 * a), a)
newPositionWithAim (x,y,a) (Up y1)      = (x, y, a - y1)
newPositionWithAim (x,y,a) (Down y1)    = (x, y, a + y1)


solve1 :: [Direction] -> Int
solve1 l = x * y
  where
    (x,y) = foldl newPosition (0,0) l

solve2 :: [Direction] -> Int
solve2 l = x * y
  where
    (x,y,_) = foldl newPositionWithAim (0,0,0) l


main :: IO ()
main = do
  input <- lines <$> readFile "input"
  let directions = (map fromStr input)
      ex1 = solve1 directions
      ex2 = solve2 directions
  print ex1
  print ex2
