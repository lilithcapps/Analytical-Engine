{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

import           Control.Lens             (ix, (&), (.~))
import           Control.Monad.State.Lazy
import           Data.Char                (intToDigit)
import qualified Data.Text.Lazy           as T
import qualified Data.Text.Lazy.IO        as IO

type DistributivePunchCard = NumberPunchCard
type NumericPunchCard = NumberPunchCard
type NumberPunchCard = PunchCard

type OperationPunchCard = PunchCard

type PunchCard = [PunchCardLine]
type PunchCardLine = String

type UnboundOperation = Either MathsOperation UnboundVariableOperation
type PairedOperation = Either MathsOperation (Int, UnboundVariableOperation)
type Operation = Either MathsOperation VariableOperation

type MillValue = (Integer, Integer)
type StoreValue = [Integer]
type Primacy = Bool

type EngineState = (MillValue, MathsOperation, StoreValue, Primacy)

data MathsOperation = Add | Subtract | Multiply | Divide
  deriving Show
data UnboundVariableOperation =
  UnboundSupplyRetaining
  | UnboundSupplyZeroing
  | UnboundStore
  | UnboundStorePrimed
  | UnboundForwards
  | UnboundBackwards
  deriving Show
data VariableOperation =
  SupplyRetaining Int
  | SupplyZeroing Int
  | Store Int
  | StorePrimed Int
  | Forwards Int
  | Backwards Int
  deriving Show

applyParameters :: [Operation] -> [Integer] -> [StoreValue]
applyParameters ops params = states
  where
    store = params ++ [0,0..] :: StoreValue
    mill = (0, 0) :: MillValue

    states :: [StoreValue]
    states = evalState (mapM engine ops) (mill, Add, store, False)
      where
        engine :: Operation -> State EngineState StoreValue
        engine op = get
          >>= put . doOperation op
          >> get
          >>= (\(_, _, s, _) -> return s)

    doOperation :: Operation -> EngineState -> EngineState
    doOperation op s@((a, b), _, store, primed) = case (op, primed) of
      (Right (SupplyRetaining n), True) -> doArithmetic . doDistributive (SupplyRetaining n) $ s
      (Right (SupplyZeroing n), True) -> doArithmetic . doDistributive (SupplyRetaining n) $ s
      (Right r, _) -> doDistributive r s
      (Left l, _) -> ((a, b), l, store, primed)
      where
        doDistributive :: VariableOperation -> EngineState -> EngineState
        doDistributive op s@((a, b), mathOp, store, primed) = case op of
          SupplyRetaining n -> if primed then ((store !! n, b), mathOp, store, not primed) else ((a, store !! n), mathOp, store, not primed)
          SupplyZeroing n   -> if primed then ((store !! n, b), mathOp, store & ix n .~ 0, not primed) else ((a, store !! n), mathOp, store & ix n .~ 0, not primed)
          Store n           -> ((0, b), mathOp, store & ix n .~ a, primed)
          StorePrimed n     -> ((a, 0), mathOp, store & ix n .~ b, primed)
          Forwards _        -> s
          Backwards _       -> s

        doArithmetic :: EngineState -> EngineState
        doArithmetic ((a, b), op, store, primed) =
          let mill = case op of
                Add      -> (a + b, 0)
                Subtract -> (a - b, 0)
                Multiply -> (a * b, 0)
                Divide   -> (a `div` b, a `mod` b)
          in (mill, op, store, primed)

    arithmeticOps :: [MathsOperation]
    arithmeticOps = [(case op of
      Left l  -> l
      Right _ -> last arithmeticOps)
      | op <- ops]

    distributiveOps :: [Either Int VariableOperation]
    distributiveOps = [ (case op of
        Left _  -> Left 0
        Right r -> Right r)
        | op <- ops]

createOperationChain :: [UnboundOperation] -> [Int] -> [Operation]
createOperationChain us vs = operationChain
  where
  -- this is lefties alone and righties as data with variable
  operations :: [Operation]
  operations = fmap bindVariableOperation <$> evalState (mapM pairVars us) vs
    where
      pairVars :: UnboundOperation -> State [Int] PairedOperation
      pairVars (Left l) =
        get >>= \vars -> case vars of
            [] -> error "ran out of vars :("
            _  -> return (vars, Left l)
            >>= \(v, e) -> put v >> return e
      pairVars (Right r) =
        get >>= \vars -> case vars of
            []     -> error "ran out of vars :("
            v:vs -> return (vs, Right (v, r))
          >>= \(v, e) -> put v >> return e

      bindVariableOperation :: (Int, UnboundVariableOperation) -> VariableOperation
      bindVariableOperation (n, UnboundSupplyRetaining) = SupplyRetaining n
      bindVariableOperation (n, UnboundSupplyZeroing)   = SupplyZeroing n
      bindVariableOperation (n, UnboundStore)           = Store n
      bindVariableOperation (n, UnboundStorePrimed)     = StorePrimed n
      bindVariableOperation (n, UnboundForwards)        = Forwards n
      bindVariableOperation (n, UnboundBackwards)       = Backwards n

  -- create infinite list of operations
  operationChain = (\(x, _, _) -> x) <$> go
    where
      go :: [(Operation, [Operation], [Operation])]
      go = (head operations, [], tail operations) : more go
        where
          more :: [(Operation, [Operation], [Operation])] -> [(Operation, [Operation], [Operation])]
          more ((c@(Right (Forwards n)), ps, ns):_) =
            let nextTup = ((!! max 0 n) ns, reverse (take n ns) ++ (c : ps), tail . drop n $ ns) : more nextTup
            in nextTup
          more ((c@(Right (Backwards n)), ps, ns):_) =
            let newPrevs = c : ps in
            let nextTup = ((!! max 0 (n-1)) newPrevs, drop n newPrevs, reverse (take (n-1) newPrevs) ++ ns) : more nextTup
            in nextTup
          more ((c, ps, n:ns):_) =
            let nextTup = (n, c:ps, ns) : more nextTup
            in nextTup
          more _ = []

main :: IO ()
main = do
  arithmeticString <- readCard "programs/default/operations.pc"
  numericFile <- readCard "programs/default/numbers.pc"
  distributiveFile <- readCard "programs/default/loadStore.pc"

  let operations :: [UnboundOperation]
      operations = parseOperation . lines . T.unpack <$> T.splitOn "-\r\n" arithmeticString
  let distributive :: [Int]
      distributive = parseVariable . lines . T.unpack <$> T.splitOn "-\r\n" distributiveFile
  let numbers :: [Integer]
      numbers = parseNumeric . lines . T.unpack <$> T.splitOn "-\r\n" numericFile

  let storeValues = applyParameters (createOperationChain operations distributive) numbers

  let inputOps = [Left Add, Right UnboundForwards, Left Divide, Right UnboundBackwards]
  let inputVars = [1, 3]
  let inputParams = [0, 0]

  let opChain = createOperationChain inputOps inputVars
  let computedValues = applyParameters opChain inputParams

  putStrLn . (++) "Initial Operations: " $ show . prettyPrintEither $ inputOps
  putStrLn $ "Initial Variables: " ++ show inputVars
  putStrLn $ "Initial Paramters: " ++ show inputParams

  print . prettyPrintEither . take 10 $ opChain
  print . take 10 $ computedValues
  return ()
  where
    readCard :: FilePath -> IO T.Text
    readCard = IO.readFile

    prettyPrintEither :: (Monad m, Show a, Show b) => m (Either a b) -> m String
    prettyPrintEither = (either show show <$>)

    parseOperation :: OperationPunchCard -> UnboundOperation
    parseOperation card =
      if | card == addCard           -> Left Add
          | card == subtractCard     -> Left Subtract
          | card == multiplyCard     -> Left Multiply
          | card == divideCard       -> Left Divide
          | card == loadPreserveCard -> Right UnboundSupplyRetaining
          | card == loadZeroCard     -> Right UnboundSupplyZeroing
          | card == storeCard        -> Right UnboundStore
          | card == storePrimedCard  -> Right UnboundStorePrimed
          | card == forwardsCard     -> Right UnboundForwards
          | card == backwardsCard    -> Right UnboundBackwards
          | True                     -> error "unknown operation card"
      where
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

    parseVariable :: DistributivePunchCard -> Int
    parseVariable = read . parseCardNumber

    parseNumeric :: NumericPunchCard -> Integer
    parseNumeric []   = error "empty punchcard"
      -- remove the first char from each line of the card
    parseNumeric card = read $ parseSign card : parseCardNumber (tail <$> card) -- this removes the leading space in numeric cards

    parseSign [] = error "empty punchcard"
    parseSign card = case head . head $ card of
      '*' -> '-'
      ' ' -> '+'
      _   -> error "badly formatted punchcard"

    parseCardNumber :: NumberPunchCard -> String
    parseCardNumber cardLines =
      [ intToDigit (col + 1)
        | row <- [0..length . head $ cardLines]
        , col <- [0..length cardLines]
        , let val = cardLines !! row !! col
        , val == '*'
      ]
