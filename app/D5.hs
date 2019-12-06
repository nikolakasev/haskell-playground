module D5 where

import           Data.List.Split

input
  --"3,0,101,10,0,0,4,0,99"
  --"1,0,0,3,1,1,2,3,1,3,4,3,1,5,0,3,2,6,1,19,1,19,5,23,2,9,23,27,1,5,27,31,1,5,31,35,1,35,13,39,1,39,9,43,1,5,43,47,1,47,6,51,1,51,13,55,1,55,9,59,1,59,13,63,2,63,13,67,1,67,10,71,1,71,6,75,2,10,75,79,2,10,79,83,1,5,83,87,2,6,87,91,1,91,6,95,1,95,13,99,2,99,13,103,1,103,9,107,1,10,107,111,2,111,13,115,1,10,115,119,1,10,119,123,2,13,123,127,2,6,127,131,1,13,131,135,1,135,2,139,1,139,6,0,99,2,0,14,0"
 =
  "3,225,1,225,6,6,1100,1,238,225,104,0,1,192,154,224,101,-161,224,224,4,224,102,8,223,223,101,5,224,224,1,223,224,223,1001,157,48,224,1001,224,-61,224,4,224,102,8,223,223,101,2,224,224,1,223,224,223,1102,15,28,225,1002,162,75,224,1001,224,-600,224,4,224,1002,223,8,223,1001,224,1,224,1,224,223,223,102,32,57,224,1001,224,-480,224,4,224,102,8,223,223,101,1,224,224,1,224,223,223,1101,6,23,225,1102,15,70,224,1001,224,-1050,224,4,224,1002,223,8,223,101,5,224,224,1,224,223,223,101,53,196,224,1001,224,-63,224,4,224,102,8,223,223,1001,224,3,224,1,224,223,223,1101,64,94,225,1102,13,23,225,1101,41,8,225,2,105,187,224,1001,224,-60,224,4,224,1002,223,8,223,101,6,224,224,1,224,223,223,1101,10,23,225,1101,16,67,225,1101,58,10,225,1101,25,34,224,1001,224,-59,224,4,224,1002,223,8,223,1001,224,3,224,1,223,224,223,4,223,99,0,0,0,677,0,0,0,0,0,0,0,0,0,0,0,1105,0,99999,1105,227,247,1105,1,99999,1005,227,99999,1005,0,256,1105,1,99999,1106,227,99999,1106,0,265,1105,1,99999,1006,0,99999,1006,227,274,1105,1,99999,1105,1,280,1105,1,99999,1,225,225,225,1101,294,0,0,105,1,0,1105,1,99999,1106,0,300,1105,1,99999,1,225,225,225,1101,314,0,0,106,0,0,1105,1,99999,1108,226,226,224,102,2,223,223,1005,224,329,101,1,223,223,107,226,226,224,1002,223,2,223,1005,224,344,1001,223,1,223,107,677,226,224,102,2,223,223,1005,224,359,101,1,223,223,7,677,226,224,102,2,223,223,1005,224,374,101,1,223,223,108,226,226,224,102,2,223,223,1006,224,389,101,1,223,223,1007,677,677,224,102,2,223,223,1005,224,404,101,1,223,223,7,226,677,224,102,2,223,223,1006,224,419,101,1,223,223,1107,226,677,224,1002,223,2,223,1005,224,434,1001,223,1,223,1108,226,677,224,102,2,223,223,1005,224,449,101,1,223,223,108,226,677,224,102,2,223,223,1005,224,464,1001,223,1,223,8,226,677,224,1002,223,2,223,1005,224,479,1001,223,1,223,1007,226,226,224,102,2,223,223,1006,224,494,101,1,223,223,1008,226,677,224,102,2,223,223,1006,224,509,101,1,223,223,1107,677,226,224,1002,223,2,223,1006,224,524,1001,223,1,223,108,677,677,224,1002,223,2,223,1005,224,539,1001,223,1,223,1107,226,226,224,1002,223,2,223,1006,224,554,1001,223,1,223,7,226,226,224,1002,223,2,223,1006,224,569,1001,223,1,223,8,677,226,224,102,2,223,223,1006,224,584,101,1,223,223,1008,677,677,224,102,2,223,223,1005,224,599,101,1,223,223,1007,226,677,224,1002,223,2,223,1006,224,614,1001,223,1,223,8,677,677,224,1002,223,2,223,1005,224,629,101,1,223,223,107,677,677,224,102,2,223,223,1005,224,644,101,1,223,223,1108,677,226,224,102,2,223,223,1005,224,659,101,1,223,223,1008,226,226,224,102,2,223,223,1006,224,674,1001,223,1,223,4,223,99,226"
 --"3,3,1105,-1,9,1101,0,0,12,4,12,99,1"

delimiter = ','

-- reads right to left
opCode :: [Int]
opCode = map (read :: String -> Int) $ splitOn "," input

main :: IO ()
-- <| in Elm
main = computer (0, opCode)

data Continuation
  = Pointer Int
  | Stop

data Operation
  = Add Operand Operand Int
  | Multiply Operand Operand Int
  | Input Int
  | Output Int
  | JumpIfTrue Operand Operand
  | JumpIfFalse Operand Operand
  | LessThan Operand Operand Int
  | Equals Operand Operand Int
  | Halt
  deriving (Eq, Show)

data Operand
  = Immediate Int
  | Address Int
  deriving (Eq, Show)

computer :: (Int, [Int]) -> IO ()
computer state = do
  op <- parse state
  (continuation, opCode') <- interpret op state
  case continuation of
    Stop             -> putStrLn "End."
    Pointer pointer' -> computer (pointer', opCode')

parse :: (Int, [Int]) -> IO Operation
-- no extra validation needed because if any of the following operations returns a Nothing, the "do" will short-circuit and return a Nothing
parse state@(pointer, opCode) = do
  raw <- getAtIndex pointer opCode
  let a@(_, mod2, mod1, op) = parseOperand raw
  --putStrLn $ "PARSED " ++ show raw
  case op of
    1  -> buildWith3Params Add (mod1, mod2) state
    2  -> buildWith3Params Multiply (mod1, mod2) state
    3  -> buildWith1Param Input state
    4  -> buildWith1Param Output state
    5  -> buildWith2Params JumpIfTrue (mod1, mod2) state
    6  -> buildWith2Params JumpIfFalse (mod1, mod2) state
    7  -> buildWith3Params LessThan (mod1, mod2) state
    8  -> buildWith3Params Equals (mod1, mod2) state
    99 -> return Halt
  where
    parseOperand :: Int -> (Int, Int, Int, Int)
    parseOperand raw =
      let a = div raw 10000
          b = div (raw - a * 10000) 1000
          c = div (raw - a * 10000 - b * 1000) 100
          d = raw - a * 10000 - b * 1000 - c * 100
       in (a, b, c, d)
    fromMode :: Int -> Int -> Operand
    fromMode 1 value = Immediate value
    fromMode _ value = Address value
    buildWith1Param :: (Int -> a) -> (Int, [Int]) -> IO a
    buildWith1Param constructor (pointer, opCode) = do
      first <- getAtIndex (pointer + 1) opCode
      return $ constructor first
    buildWith2Params :: (Operand -> Operand -> a) -> (Int, Int) -> (Int, [Int]) -> IO a
    buildWith2Params constructor (mod1, mod2) (pointer, opCode) = do
      first <- getAtIndex (pointer + 1) opCode
      second <- getAtIndex (pointer + 2) opCode
      return $ constructor (fromMode mod1 first) (fromMode mod2 second)
    buildWith3Params :: (Operand -> Operand -> Int -> a) -> (Int, Int) -> (Int, [Int]) -> IO a
    buildWith3Params constructor (mod1, mod2) (pointer, opCode) = do
      first <- getAtIndex (pointer + 1) opCode
      second <- getAtIndex (pointer + 2) opCode
      to <- getAtIndex (pointer + 3) opCode
      return $ constructor (fromMode mod1 first) (fromMode mod2 second) to

interpret :: Operation -> (Int, [Int]) -> IO (Continuation, [Int])
interpret op (pointer, opCode) =
  case op of
    Add first second to -> do
      a <- getOperandValue first opCode
      b <- getOperandValue second opCode
      opCode' <- setAtIndex to (a + b) opCode
      return (Pointer $ pointer + 4, opCode')
    Multiply first second to -> do
      a <- getOperandValue first opCode
      b <- getOperandValue second opCode
      opCode' <- setAtIndex to (a * b) opCode
      return (Pointer $ pointer + 4, opCode')
    Input to -> do
      input <- readLn
      opCode' <- setAtIndex to input opCode
      return (Pointer $ pointer + 2, opCode')
    Output from -> do
      a <- getAtIndex from opCode
      print a
      return (Pointer $ pointer + 2, opCode)
    -- jumping doesn't modify the Intcode, it only affects the instruction pointer
    JumpIfTrue first second -> do
      a <- getOperandValue first opCode
      pointer' <-
        if a /= 0
          then getOperandValue second opCode
          -- move the pointer if not jumping
          else return (pointer + 3)
      return (Pointer pointer', opCode)
    JumpIfFalse first second -> do
      a <- getOperandValue first opCode
      -- `pointer` is already an IO Int
      pointer' <-
        if a == 0
          then getOperandValue second opCode
          else return (pointer + 3)
      return (Pointer pointer', opCode)
    LessThan first second to -> do
      a <- getOperandValue first opCode
      b <- getOperandValue second opCode
      let value =
            if a < b
              then 1
              else 0
      opCode' <- setAtIndex to value opCode
      return (Pointer $ pointer + 4, opCode')
    Equals first second to -> do
      a <- getOperandValue first opCode
      b <- getOperandValue second opCode
      let value
      -- TODO the only difference with LessThan is the function `==` instead of `<`, how to refactor?
           =
            if a == b
              then 1
              else 0
      opCode' <- setAtIndex to value opCode
      return (Pointer $ pointer + 4, opCode')
    Halt -> return (Stop, opCode)
  where
    getOperandValue :: Operand -> [Int] -> IO Int
    getOperandValue (Immediate x) _    = return x
    getOperandValue (Address x) opCode = getAtIndex x opCode

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
