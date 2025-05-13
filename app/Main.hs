{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ParallelListComp  #-}

import           Data.Char         (intToDigit)
import qualified Data.Text.Lazy    as T
import qualified Data.Text.Lazy.IO as IO

type VariablePunchCard = NumberPunchCard
type NumericPunchCard = NumberPunchCard
type NumberPunchCard = PunchCard

type OperationPunchCard = PunchCard

type PunchCard = [PunchCardLine]
type PunchCardLine = String

type Operation = Either MathsOperation VariableOperation
type RightPairedOperation = Either MathsOperation (Int, VariableOperation)
type TuplePairedOperation = ([Int], Either MathsOperation VariableOperation)

type MathsOperation = Either ArithmeticOperation DivisionOperation
type VariableOperation = Either StoreOperation MoveOperation

type ArithmeticOperation = Integer -> Integer -> Integer
type DivisionOperation = Integer -> Integer -> (Integer, Integer)
data StoreOperation = LoadPreserve | LoadZero | Store | StorePrimed
type MoveOperation = Int -> Int


process operationCards numericCards variableCards = do
  let numbers :: [Integer]
      numbers = map parseNumeric numericCards
  let variables :: [Int]
      variables = map (read . parseCardNumber) variableCards

  let operations = map parseOperation operationCards

  return (test' operations numbers variables)
  --return process' (Left (+)) [] operations [] numbers [] variables

test' :: [Operation] -> [Integer] -> [Int] -> m a2
test' operations numbers variables = do

  -- split
  -- then pair, using list of vars passed in, and returning the tail when its a real value
  -- then parallel iterate arith n ldst
  -- maybe have one of them going backwards ?

  let fullyPairedOps :: [TuplePairedOperation]
      -- the magic: 0 gets dropped when we actually first
      -- need a val, leaving us with a tail whose head is the value we want
      -- (+) is our starting value, meaning we don't have to set it anywhere
      fullyPairedOps = (0:variables, Left . Left $ (+)) : [
        (case op of
          Left l  ->        (fst . last $ fullyPairedOps, Left l)
          Right r -> (tail . fst . last $ fullyPairedOps, Right r)
          )| op <- operations]


  -- this is lefties alone and righties with their var
  let opsWVars :: [RightPairedOperation]
      opsWVars = [ (case op of
        (_, Left l)    -> Left l
        (v:_, Right r) -> Right (v, r)
        ([], Right _)  -> error "variables terminated early")
        | op <- fullyPairedOps]

  -- now do skip back and forth processing
  let singleStream :: [Either MathsOperation (Int, StoreOperation)]
      singleStream = concat [ case op of
        (idx, Right (var, Right rr)) -> parseMoveOp idx (rr var)
        (_, Right (var, Left lr))    -> [Right (var, lr)]
        (_, Left l)                  -> [Left l]
          | op <- zip [0..] opsWVars]
          where
            parseMoveOp :: Int -> Int -> [Either MathsOperation (Int, StoreOperation)]
            parseMoveOp idx op =
              if  | op > 0 -> [singleStream !! (idx+index) |index <- [1..op]]
                  | op < 0 -> [opsWVars !! (idx+index) | index <- [op,(op-1)..(-1)]]
                  | True   -> error "variable cannot be 0 for a movement operation"





  -- then do full pass of process

  -- .. numbers? ... stored vars? .... the storeeeee?

  let arithmeticOps :: [MathsOperation]
      arithmeticOps = Left (+) : [ (case op of
        Left l  -> l
        Right _ -> last arithmeticOps)
        | op <- operations]

  let loadStoreOps = -1 : [ (case op of
        Left _ -> -1
        Right r -> case r of
          Left rl  -> undefined
          Right rr -> undefined {- do load store stuff???? -})
        | op <- operations]

  undefined

{-
process' :: Either ArithmeticOperation DivisionOperation -> [Operation] -> [Operation] -> [Integer] -> [Integer] -> [Int] -> [Int] -> a
process' _ _ [] _ _ _ _ = 0
process' _ _ (_:_) _ [] _ _ = 0
process' _ _ (_:_) _ (_:_) _ [] = 0
-- prevs are LIFO lists i.e. the first item is the var we just processed
process' selectedOp prevOps nextOps@(op:ops) prevNums nextNums@(num:nums) prevVars nextVars@(var:vars)
 = case op of
    Left l -> process' l prevOps nextOps prevNums nextNums prevVars nextVars
    Right r -> case r of
      -- Store Op
      Left rl  -> case rl of
        0 -> undefined -- loadPreservelogic
        1 -> undefined -- loadZerologic
        2 -> undefined -- storelogic
        3 -> undefined -- storePrimedlogic
        _ -> error "bad value"
      -- Movement Op
      Right rr ->
        if | result > 0 -> process' selectedOp (reverse (take (var + 1) nextOps) ++ prevOps) (drop (var + 1) nextOps) prevNums nextNums prevVars nextVars
           | result < 0 -> process' selectedOp (drop (var - 1) prevOps) (reverse (take (var - 1) prevOps) ++ nextOps) prevNums nextNums prevVars nextVars
           | True -> error "bad movement var"
           where
            result = rr var
-}

parseOperation :: OperationPunchCard -> Operation
parseOperation card =
  if | card == addCard          -> Left  . Left  $ (+)
     | card == subtractCard     -> Left  . Left  $ (-)
     | card == multiplyCard     -> Left  . Left  $ (*)
     | card == divideCard       -> Left  . Right $ div'
     | card == loadPreserveCard -> Right . Left  $ LoadPreserve
     | card == loadZeroCard     -> Right . Left  $ LoadZero
     | card == storeCard        -> Right . Left  $ Store
     | card == storePrimedCard  -> Right . Left  $ StorePrimed
     | card == forwardsCard     -> Right . Right $ (0+)
     | card == backwardsCard    -> Right . Right $ (0-)
     | True                     -> error "unknown operation card"
  where
    div' a b = (a `div` b, a `mod` b)
    addCard          = ["* ", "  ", "  ", "  ", "  "]
    subtractCard     = [" *", "  ", "  ", "  ", "  "]
    multiplyCard     = ["  ", "* ", "  ", "  ", "  "]
    divideCard       = ["  ", " *", "  ", "  ", "  "]
    loadPreserveCard = ["  ", "  ", "* ", "  ", "  "]
    loadZeroCard     = ["  ", "  ", " *", "  ", "  "]
    storeCard        = ["  ", "  ", "  ", "* ", "  "]
    storePrimedCard  = ["  ", "  ", "  ", " *", "  "]
    forwardsCard     = ["* ", "  ", "  ", "  ", "* "]
    backwardsCard    = [" *", "  ", "  ", "  ", "* "]


parseNumeric :: NumericPunchCard -> Integer
parseNumeric [] = error "empty punchcard"
parseNumeric card = case head . head $ card of
  '*' -> read $ '-' : parseCardNumber (tail <$> card :: NumericPunchCard) -- remove the first char from each line of the card
  ' ' -> read $ '+' : parseCardNumber (tail <$> card :: NumericPunchCard) -- this removes the leading space in numeric cards
  _   -> error "badly formatted punchcard"

parseCardNumber :: NumberPunchCard -> String
parseCardNumber cardLines =
  [ intToDigit (col + 1)
    | row <- [0..length . head $ cardLines]
    , col <- [0..length cardLines]
    , let val = cardLines !! row !! col
    , val == '*'
  ]

readCard :: FilePath -> IO T.Text
readCard loc = IO.readFile loc


main = do
  arithmeticString <- readCard "default/operations.pc"
  numericFile <- readCard "default/numbers.pc"
  loadStoreFile <- readCard "default/loadStore.pc"

  let operations :: [OperationPunchCard]
      operations = map (lines . T.unpack) $ T.splitOn "-\r\n" arithmeticString
  let numbers :: [NumericPunchCard]
      numbers = map (lines . T.unpack) $ T.splitOn "-\r\n" numericFile
  let variables :: [VariablePunchCard]
      variables = map (lines . T.unpack) $ T.splitOn "-\r\n" loadStoreFile

  return undefined
  --return $ process operations numbers variables
