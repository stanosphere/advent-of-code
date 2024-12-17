module Day17 where

import Data.Bits (xor)
import Data.List.Extra ((!?))
import qualified Text.Parsec as P
import Text.ParserCombinators.Parsec (Parser, parse)

data Machine = Machine {_a :: Int, _b :: Int, _c :: Int, _output :: [Int], _pointer :: Int} deriving (Show)

part1 :: IO ()
part1 = do
  (machine, program) <- getInput
  let res = run program machine
  print . _output $ res

run :: [Int] -> Machine -> Machine
run program machine =
  case (program !? index, program !? (index + 1)) of
    (Just _, Just _) -> run' program machine
    _ -> machine
  where
    index = _pointer machine

run' :: [Int] -> Machine -> Machine
run' program machine = run program newState
  where
    newState = case operator of
      0 -> run0 operand machine
      1 -> run1 operand machine
      2 -> run2 operand machine
      3 -> run3 operand machine
      4 -> run4 operand machine
      5 -> run5 operand machine
      6 -> run6 operand machine
      7 -> run7 operand machine
      _ -> error ("unexpected operator: " ++ show operator)
    operator = program !! _pointer machine
    operand = program !! (_pointer machine + 1)

-- The adv instruction (opcode 0) performs division.
-- The numerator is the value in the A register.
-- The denominator is found by raising 2 to the power of the instruction's combo operand. (So, an operand of 2 would divide A by 4 (2^2); an operand of 5 would divide A by 2^B.)
-- The result of the division operation is truncated to an integer and then written to the A register.
run0 :: Int -> Machine -> Machine
run0 comboOperand m = m {_a = result, _pointer = 2 + _pointer m}
  where
    numerator = _a m
    denominator = 2 ^ resolveComboOperand m comboOperand
    result = floor (asDouble numerator / asDouble denominator)

-- The bxl instruction (opcode 1) calculates the bitwise XOR of register B and the instruction's literal operand,
-- then stores the result in register B.
run1 :: Int -> Machine -> Machine
run1 literalOperand m = m {_b = result, _pointer = 2 + _pointer m}
  where
    result = xor (_b m) literalOperand

-- The bst instruction (opcode 2) calculates the value of its combo operand modulo 8 (thereby keeping only its lowest 3 bits),
-- then writes that value to the B register.
run2 :: Int -> Machine -> Machine
run2 comboOperand m = m {_b = result, _pointer = 2 + _pointer m}
  where
    result = resolveComboOperand m comboOperand `mod` 8

-- The jnz instruction (opcode 3) does nothing if the A register is 0.
-- However, if the A register is not zero, it jumps by setting the instruction pointer to the value of its literal operand; if this instruction jumps, the instruction pointer is not increased by 2 after this instruction.
run3 :: Int -> Machine -> Machine
run3 literalOperand m =
  if _a m == 0
    then m {_pointer = 2 + _pointer m}
    else m {_pointer = literalOperand}

-- The bxc instruction (opcode 4) calculates the bitwise XOR of register B and register C,
-- then stores the result in register B. (For legacy reasons, this instruction reads an operand but ignores it.)
run4 :: Int -> Machine -> Machine
run4 _ m = m {_b = result, _pointer = 2 + _pointer m}
  where
    result = xor (_b m) (_c m)

-- The out instruction (opcode 5) calculates the value of its combo operand modulo 8,
-- then outputs that value. (If a program outputs multiple values, they are separated by commas.)
run5 :: Int -> Machine -> Machine
run5 comboOperand m = m {_output = _output m ++ [result], _pointer = 2 + _pointer m}
  where
    result = resolveComboOperand m comboOperand `mod` 8

-- The bdv instruction (opcode 6) works exactly like the adv instruction except that the result is stored in the B register. (The numerator is still read from the A register.)
run6 :: Int -> Machine -> Machine
run6 comboOperand m = m {_b = result, _pointer = 2 + _pointer m}
  where
    numerator = _a m
    denominator = 2 ^ resolveComboOperand m comboOperand
    result = floor (asDouble numerator / asDouble denominator)

-- The cdv instruction (opcode 7) works exactly like the adv instruction except that the result is stored in the C register. (The numerator is still read from the A register.)
run7 :: Int -> Machine -> Machine
run7 comboOperand m = m {_c = result, _pointer = 2 + _pointer m}
  where
    numerator = _a m
    denominator = 2 ^ resolveComboOperand m comboOperand
    result = floor (asDouble numerator / asDouble denominator)

asDouble :: (Integral a) => a -> Double
asDouble = fromIntegral

-- Combo operands 0 through 3 represent literal values 0 through 3.
-- Combo operand 4 represents the value of register A.
-- Combo operand 5 represents the value of register B.
-- Combo operand 6 represents the value of register C.
-- Combo operand 7 is reserved and will not appear in valid programs
resolveComboOperand :: Machine -> Int -> Int
resolveComboOperand _ 0 = 0
resolveComboOperand _ 1 = 1
resolveComboOperand _ 2 = 2
resolveComboOperand _ 3 = 3
resolveComboOperand m 4 = _a m
resolveComboOperand m 5 = _b m
resolveComboOperand m 6 = _c m
resolveComboOperand _ x = error ("unexpected operand: " ++ show x)

-- example 1: If register C contains 9, the program 2,6 would set register B to 1.
example1 :: Machine
example1 = run [2, 6] (Machine 0 0 9 [] 0)

-- example 2: If register A contains 10, the program 5,0,5,1,5,4 would output 0,1,2.
example2 :: Machine
example2 = run [5, 0, 5, 1, 5, 4] (Machine 10 0 0 [] 0)

-- example 3: If register A contains 2024, the program 0,1,5,4,3,0 would output 4,2,5,6,7,7,7,7,3,1,0 and leave 0 in register A.

example3 :: Machine
example3 = run [0, 1, 5, 4, 3, 0] (Machine 2024 0 0 [] 0)

-- example 4: If register B contains 29, the program 1,7 would set register B to 26.
-- example 5: If register B contains 2024 and register C contains 43690, the program 4,0 would set register B to 44354.

getInput :: IO (Machine, [Int])
getInput = unsafeParse inputParser <$> readFile "./fixtures/input17.txt"

unsafeParse :: Parser a -> String -> a
unsafeParse p s = case parse p "" s of
  Left res -> error . show $ res
  Right res -> res

inputParser :: Parser (Machine, [Int])
inputParser = do
  _ <- P.string "Register A: "
  a <- intParser
  _ <- P.newline
  _ <- P.string "Register B: "
  b <- intParser
  _ <- P.newline
  _ <- P.string "Register C: "
  c <- intParser
  _ <- P.newline
  _ <- P.newline
  _ <- P.string "Program: "
  program <- intParser `P.sepBy` P.char ','
  return (Machine a b c [] 0, program)
  where
    intParser :: Parser Int
    intParser = read <$> P.many1 (P.digit P.<|> P.char '-')