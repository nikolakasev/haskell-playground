module D5 where

import           Data.List.Split

input
  --"3,0,101,10,0,0,4,0,99"
  --"1,0,0,3,1,1,2,3,1,3,4,3,1,5,0,3,2,6,1,19,1,19,5,23,2,9,23,27,1,5,27,31,1,5,31,35,1,35,13,39,1,39,9,43,1,5,43,47,1,47,6,51,1,51,13,55,1,55,9,59,1,59,13,63,2,63,13,67,1,67,10,71,1,71,6,75,2,10,75,79,2,10,79,83,1,5,83,87,2,6,87,91,1,91,6,95,1,95,13,99,2,99,13,103,1,103,9,107,1,10,107,111,2,111,13,115,1,10,115,119,1,10,119,123,2,13,123,127,2,6,127,131,1,13,131,135,1,135,2,139,1,139,6,0,99,2,0,14,0"
 =
  "3,225,1,225,6,6,1100,1,238,225,104,0,1,192,154,224,101,-161,224,224,4,224,102,8,223,223,101,5,224,224,1,223,224,223,1001,157,48,224,1001,224,-61,224,4,224,102,8,223,223,101,2,224,224,1,223,224,223,1102,15,28,225,1002,162,75,224,1001,224,-600,224,4,224,1002,223,8,223,1001,224,1,224,1,224,223,223,102,32,57,224,1001,224,-480,224,4,224,102,8,223,223,101,1,224,224,1,224,223,223,1101,6,23,225,1102,15,70,224,1001,224,-1050,224,4,224,1002,223,8,223,101,5,224,224,1,224,223,223,101,53,196,224,1001,224,-63,224,4,224,102,8,223,223,1001,224,3,224,1,224,223,223,1101,64,94,225,1102,13,23,225,1101,41,8,225,2,105,187,224,1001,224,-60,224,4,224,1002,223,8,223,101,6,224,224,1,224,223,223,1101,10,23,225,1101,16,67,225,1101,58,10,225,1101,25,34,224,1001,224,-59,224,4,224,1002,223,8,223,1001,224,3,224,1,223,224,223,4,223,99,0,0,0,677,0,0,0,0,0,0,0,0,0,0,0,1105,0,99999,1105,227,247,1105,1,99999,1005,227,99999,1005,0,256,1105,1,99999,1106,227,99999,1106,0,265,1105,1,99999,1006,0,99999,1006,227,274,1105,1,99999,1105,1,280,1105,1,99999,1,225,225,225,1101,294,0,0,105,1,0,1105,1,99999,1106,0,300,1105,1,99999,1,225,225,225,1101,314,0,0,106,0,0,1105,1,99999,1108,226,226,224,102,2,223,223,1005,224,329,101,1,223,223,107,226,226,224,1002,223,2,223,1005,224,344,1001,223,1,223,107,677,226,224,102,2,223,223,1005,224,359,101,1,223,223,7,677,226,224,102,2,223,223,1005,224,374,101,1,223,223,108,226,226,224,102,2,223,223,1006,224,389,101,1,223,223,1007,677,677,224,102,2,223,223,1005,224,404,101,1,223,223,7,226,677,224,102,2,223,223,1006,224,419,101,1,223,223,1107,226,677,224,1002,223,2,223,1005,224,434,1001,223,1,223,1108,226,677,224,102,2,223,223,1005,224,449,101,1,223,223,108,226,677,224,102,2,223,223,1005,224,464,1001,223,1,223,8,226,677,224,1002,223,2,223,1005,224,479,1001,223,1,223,1007,226,226,224,102,2,223,223,1006,224,494,101,1,223,223,1008,226,677,224,102,2,223,223,1006,224,509,101,1,223,223,1107,677,226,224,1002,223,2,223,1006,224,524,1001,223,1,223,108,677,677,224,1002,223,2,223,1005,224,539,1001,223,1,223,1107,226,226,224,1002,223,2,223,1006,224,554,1001,223,1,223,7,226,226,224,1002,223,2,223,1006,224,569,1001,223,1,223,8,677,226,224,102,2,223,223,1006,224,584,101,1,223,223,1008,677,677,224,102,2,223,223,1005,224,599,101,1,223,223,1007,226,677,224,1002,223,2,223,1006,224,614,1001,223,1,223,8,677,677,224,1002,223,2,223,1005,224,629,101,1,223,223,107,677,677,224,102,2,223,223,1005,224,644,101,1,223,223,1108,677,226,224,102,2,223,223,1005,224,659,101,1,223,223,1008,226,226,224,102,2,223,223,1006,224,674,1001,223,1,223,4,223,99,226"

delimiter = ','

-- reads right to left
opCode :: [Int]
opCode = map (read :: String -> Int) $ splitOn "," input

main :: IO ()
-- <| in Elm
main = top 0 opCode

top :: Int -> [Int] -> IO ()
top offset opCode
      -- this is the type signature of the computation function
 =
  let computation :: IO (Int, [Int])
      computation = top' offset opCode
      handleResult :: (Int, [Int]) -> IO ()
      -- pattern matching in the body of the function
      handleResult (0, opCode') = putStrLn $ "Star 1: " ++ show (head opCode')
      handleResult (offset', opCode') = top (offset' + offset) opCode'
  -- computation.flatMap(handleResult) in Scala for example
   in computation >>= handleResult

top' :: Int -> [Int] -> IO (Int, [Int])
top' offSet opCode = do
  op <- parse offSet opCode
  -- (,) is a function that constructs a tuple
  -- partially applies the (,) function and populates the fst of the tuple
  let tupleResultWithPointerOffset = (,) $ offsetFromOperation op
  if op == Halt
    then return (0, opCode)
    else fmap tupleResultWithPointerOffset (interpreter op opCode)

data Operation
  = Add Operand Operand Int
  | Multiply Operand Operand Int
  | Input Int
  | Output Int
  | Halt
  deriving (Eq, Show)

data Operand
  = Immediate Int
  | Address Int
  deriving (Eq, Show)

fromMode :: Int -> Int -> Operand
fromMode 1 value = Immediate value
fromMode _ value = Address value

getOperandValue :: Operand -> [Int] -> IO Int
getOperandValue (Immediate x) _    = return x
getOperandValue (Address x) opCode = getAtIndex x opCode

parseOperand :: Int -> (Int, Int, Int, Int)
parseOperand raw =
  let a = div raw 10000
      b = div (raw - a * 10000) 1000
      c = div (raw - a * 10000 - b * 1000) 100
      d = raw - a * 10000 - b * 1000 - c * 100
   in (a, b, c, d)

parse :: Int -> [Int] -> IO Operation
-- no extra validation needed because if any of the following operations returns a Nothing, the "do" will short-circuit and return a Nothing
parse offset opCode = do
  let index = offset
  raw <- getAtIndex index opCode
  let a@(_, mod2, mod1, op) = parseOperand raw
  --putStrLn $ "PARSED " ++ show raw
  if op == 99
    then return Halt
    else if op < 3
           then do
             first <- getAtIndex (index + 1) opCode
             second <- getAtIndex (index + 2) opCode
             to <- getAtIndex (index + 3) opCode
             let constructor =
                   if op == 1
                     then Add
                     else Multiply
             return $ constructor (fromMode mod1 first) (fromMode mod2 second) to
           else do
             first <- getAtIndex (index + 1) opCode
             let constructor =
                   if op == 3
                     then Input
                     else Output
             return $ constructor first

-- enabling the strict compiler option should not compile if not a total function
offsetFromOperation :: Operation -> Int
offsetFromOperation Add {}      = 4
offsetFromOperation Multiply {} = 4
offsetFromOperation Input {}    = 2
offsetFromOperation Output {}   = 2
offsetFromOperation Halt        = 0

interpreter :: Operation -> [Int] -> IO [Int]
interpreter op opCode =
  case op of
    Add first second to -> do
      a <- getOperandValue first opCode
      b <- getOperandValue second opCode
      setAtIndex to (a + b) opCode
    Multiply first second to -> do
      a <- getOperandValue first opCode
      b <- getOperandValue second opCode
      setAtIndex to (a * b) opCode
    Input to -> do
      input <- readLn
      setAtIndex to input opCode
    Output from -> do
      a <- getAtIndex from opCode
      print a
      return opCode
    Halt -> return opCode

getAtIndex :: Int -> [a] -> IO a
getAtIndex index list
  | index >= length list = fail "Reading out of bounds"
  | index < 0 = fail "Reading out of bounds"
-- Nothing means an error, not mix up the semantics with something that doesn't exist
getAtIndex 0 (head:tail) = return head
getAtIndex index (head:tail) = getAtIndex (index - 1) tail

setAtIndex :: Int -> a -> [a] -> IO [a]
setAtIndex index value list
  | index >= length list = fail "Writing out of bounds"
  | index < 0 = fail "Writing out of bounds"
setAtIndex 0 a (head:tail) = return (a : tail)
setAtIndex index a (head:tail) = fmap (\list -> head : list) (setAtIndex (index - 1) a tail)
