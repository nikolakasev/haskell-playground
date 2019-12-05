module D4 where

import qualified Data.Set as Set


main :: IO ()
main = () <$ traverse print solution


solution =
  [ (a, b, c, d, e, f)
  | a <- [2 .. 7]
  , b <- [2 .. 9]
  , c <- [2 .. 9]
  , d <- [2 .. 9]
  , e <- [2 .. 9]
  , f <- [2 .. 9]
  , a <= b
  , b <= c
  , c <= d
  , d <= e
  , e <= f
  , (a == b) && (b /= c) && (b /= d) && (b /= e) && (b /= f) ||
      (b == c) && (b /= a) && (b /= d) && (b /= e) && (b /= f) ||
      (c == d) && (a /= c) && (b /= c) && (c /= e) && (c /= f) ||
      (d == e) && (a /= d) && (b /= d) && (d /= c) && (d /= f) ||
      (e == f) && (a /= e) && (b /= e) && (c /= e) && (d /= f)
  , Set.size (Set.fromList [a, b, c, d, e, f]) <= 5
  , let total = a * 10 ^ 5 + b * 10 ^ 4 + c * 10 ^ 3 + d * 10 ^ 2 + e * 10 + f
     in total >= 266666 && total <= 799999
  ]
