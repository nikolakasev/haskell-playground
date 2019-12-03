module Main where

import           Data.List.Split

input =
  "1,0,0,3,1,1,2,3,1,3,4,3,1,5,0,3,2,6,1,19,1,19,5,23,2,9,23,27,1,5,27,31,1,5,31,35,1,35,13,39,1,39,9,43,1,5,43,47,1,47,6,51,1,51,13,55,1,55,9,59,1,59,13,63,2,63,13,67,1,67,10,71,1,71,6,75,2,10,75,79,2,10,79,83,1,5,83,87,2,6,87,91,1,91,6,95,1,95,13,99,2,99,13,103,1,103,9,107,1,10,107,111,2,111,13,115,1,10,115,119,1,10,119,123,2,13,123,127,2,6,127,131,1,13,131,135,1,135,2,139,1,139,6,0,99,2,0,14,0"

delimiter = ','

-- reads right to left
opCode :: [Int]
opCode = map (read :: String -> Int) $ splitOn "," input

tweakedCode :: [Int]
tweakedCode = [head opCode] ++ [12, 2] ++ (drop 3 opCode)

main :: IO ()
-- <| in Elm
main = putStrLn $ top 0 tweakedCode

top :: Int -> [Int] -> String
top offset opCode =
  case top' offset opCode of
    Nothing               -> "Boom"
    Just (False, opCode') -> "Star 1: " ++ show (head opCode')
    Just (True, opCode')  -> top (offset + 1) opCode'

extract :: Int -> [Int] -> Maybe (Int, Int, Int, Int)
-- no extra validation needed because if any of the following operations returns a Nothing, the "do" will short-circuit and return a Nothing
extract offSet opCode = do
  index <- Just (offSet * 4)
  op <- getAtIndex index opCode
  if op == 99
    then Just (99, 0, 0, 0)
    else do
      first <- getAtIndex (index + 1) opCode
      second <- getAtIndex (index + 2) opCode
      to <- getAtIndex (index + 3) opCode
      return (op, first, second, to)

top' :: Int -> [Int] -> Maybe (Bool, [Int])
top' offSet opCode = do
  (op, first, second, to) <- extract offSet opCode
  if op == 99
    then Just (False, opCode)
    else do
      a <- getAtIndex first opCode
      b <- getAtIndex second opCode
    -- + and - are functions
      realOp <-
        if op == 1
          then Just (+)
          else Just (*)
    -- transforming to a prefix notation
      opCode' <- setAtIndex to (realOp a b) opCode
      return (True, opCode')

getAtIndex :: Int -> [a] -> Maybe a
getAtIndex index list
  | index >= length list = Nothing
  | index < 0 = Nothing
-- Nothing means an error, not mix up the semantics with something that doesn't exist
getAtIndex 0 (head:tail) = Just head
getAtIndex index (head:tail) = getAtIndex (index - 1) tail

setAtIndex :: Int -> a -> [a] -> Maybe [a]
setAtIndex index value list
  | index >= length list = Nothing
  | index < 0 = Nothing
setAtIndex 0 a (head:tail) = Just (a : tail)
setAtIndex index a (head:tail) = fmap (\list -> head : list) (setAtIndex (index - 1) a tail)
