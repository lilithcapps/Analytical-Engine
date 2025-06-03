{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

import           Control.Lens             (ix, (&), (.~))
import           Control.Monad.State.Lazy
import           Data.Char                (intToDigit)
import           Data.Maybe               (fromJust, isJust)
import qualified Data.Text.Lazy           as T
import qualified Data.Text.Lazy.IO        as IO
import           Minecart                 (Minecart, backwardsN, dismount,
                                           forwardsN, mount, next)

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

type MinecartM = Maybe (Minecart Operation [Operation] [Operation])

type EngineState = (MinecartM, MillValue, MathsOperation, StoreValue, Primacy)

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

applyParameters :: [Integer] -> [Operation] -> [StoreValue]
applyParameters params ops = states
  where
    store = params ++ [0,0..] :: StoreValue
    mill = (0, 0) :: MillValue

    states :: [StoreValue]
    states = evalState (mapM engine ops) (mount ops, mill, Add, store, False)
      where
        engine :: Operation -> State EngineState StoreValue
        engine op = get
          >>= put . doOperation op -- do the op, then put, move, get, return ??
          >> get
          >>= (\(mc, a, b, c, d) -> return (doMovement mc, a, b, c, d))
          >>= (\(_, _, _, s, _) -> return s)

        doMovement :: MinecartM -> MinecartM
        doMovement mc = case mc >>= dismount of
          Just (Right (Forwards n))  -> mc >>= next >>= forwardsN n
          Just (Right (Backwards n)) -> mc >>= backwardsN (n-1)
          Nothing                    -> Nothing
          _                          -> mc >>= next

        operationChain :: [Maybe Operation]
        operationChain = (>>= dismount) <$> go
          where
            go :: [MinecartM]
            go = mount ops : more go
              where
                more :: [MinecartM] -> [MinecartM]
                more (Nothing:_) = []
                more (mc:_) =
                  let newMc = (case mc >>= dismount of
                        Just (Right (Forwards n))  -> mc >>= next >>= forwardsN n
                        Just (Right (Backwards n)) -> mc >>= backwardsN (n-1)
                        Nothing                    -> Nothing
                        _                          -> mc >>= next
                        ) : more newMc
                  in newMc
                more _ = []

    doOperation :: Operation -> EngineState -> EngineState
    doOperation op s@(mc, (a, b), _, store, primed) = case (op, primed) of
      (Right op@(SupplyRetaining _), True) -> doArithmetic . doDistributive op $ s
      (Right op@(SupplyZeroing _), True) -> doArithmetic . doDistributive op $ s
      (Right r, _) -> doDistributive r s
      (Left l, _) -> (mc, (a, b), l, store, primed)
      where
        doDistributive :: VariableOperation -> EngineState -> EngineState
        doDistributive op s@(mc, (a, b), mathOp, store, primed) = case op of
          SupplyRetaining n -> if primed then (mc, (store !! n, b), mathOp, store, not primed)
                                         else (mc, (a, store !! n), mathOp, store, not primed)
          SupplyZeroing n   -> if primed then (mc, (store !! n, b), mathOp, store & ix n .~ 0, not primed)
                                         else (mc, (a, store !! n), mathOp, store & ix n .~ 0, not primed)
          Store n           -> (mc, (0, b), mathOp, store & ix n .~ a, primed)
          StorePrimed n     -> (mc, (a, 0), mathOp, store & ix n .~ b, primed)
          _        -> s

        doArithmetic :: EngineState -> EngineState
        doArithmetic (mc, (a, b), op, store, primed) =
          let mill = case op of
                Add      -> (a + b, 0)
                Subtract -> (a - b, 0)
                Multiply -> (a * b, 0)
                Divide   -> (a `div` b, a `mod` b)
          in (mc, mill, op, store, primed)

    -- arithmeticOps :: [MathsOperation]
    -- arithmeticOps = [(case op of
    --   Left l  -> l
    --   Right _ -> last arithmeticOps)
    --   | op <- ops]

    -- distributiveOps :: [Either Int VariableOperation]
    -- distributiveOps = [ (case op of
    --     Left _  -> Left 0
    --     Right r -> Right r)
    --     | op <- ops]

bindOperations :: [Int] -> [UnboundOperation] -> [Operation]
bindOperations vs us = operations
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
            []   -> error "ran out of vars :("
            v:vs -> return (vs, Right (v, r))
          >>= \(v, e) -> put v >> return e

      bindVariableOperation :: (Int, UnboundVariableOperation) -> VariableOperation
      bindVariableOperation (n, UnboundSupplyRetaining) = SupplyRetaining n
      bindVariableOperation (n, UnboundSupplyZeroing)   = SupplyZeroing n
      bindVariableOperation (n, UnboundStore)           = Store n
      bindVariableOperation (n, UnboundStorePrimed)     = StorePrimed n
      bindVariableOperation (n, UnboundForwards)        = Forwards n
      bindVariableOperation (n, UnboundBackwards)       = Backwards n


main :: IO ()
main = do
  let programDirectory = "programs/default/"
  arithmeticString <- readCard $ programDirectory ++ "operations.pc"
  numericFile <- readCard $ programDirectory ++ "numbers.pc"
  distributiveFile <- readCard $ programDirectory ++ "loadStore.pc"

  let operations :: [UnboundOperation]
      operations = parseOperation . lines . T.unpack <$> T.splitOn "-\r\n" arithmeticString
  let distributive :: [Int]
      distributive = parseVariable . lines . T.unpack <$> T.splitOn "-\r\n" distributiveFile
  let numbers :: [Integer]
      numbers = parseNumeric . lines . T.unpack <$> T.splitOn "-\r\n" numericFile

  let storeValues = applyParameters numbers $ bindOperations distributive operations

  let inputOps = [
        Left Add,
        Right UnboundSupplyZeroing,
        Right UnboundSupplyRetaining,
        Right UnboundStore,
        Right UnboundSupplyZeroing,
        Right UnboundSupplyZeroing,
        Right UnboundStore
        ]
  let inputVars = [0, 1, 5, 1, 5, 6]
  let inputParams = [5, 10]

  let opChain = bindOperations inputVars inputOps
  let computedValues = applyParameters inputParams opChain

  putStrLn . (++) "Initial Operations: " $ show . prettyPrintEither $ inputOps
  putStrLn $ "Initial Variables: " ++ show inputVars
  putStrLn $ "Initial Paramters: " ++ show inputParams

  print . prettyPrintEither . take 20 $ opChain
  print . take 20 . map (take 10) $ computedValues
  print . dropWhile (\n -> n !! 5 == 0) . map (take 10) $ computedValues
  return ()
  where
    readCard :: FilePath -> IO T.Text
    readCard = IO.readFile

    prettyPrintEither :: (Functor f, Show a, Show b) => f (Either a b) -> f String
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
    parseCardNumber [] = error "empty punchcard"
    parseCardNumber cardLines =
      [ intToDigit (col + 1)
        | row <- [0..length . head $ cardLines]
        , col <- [0..length cardLines]
        , let val = cardLines !! row !! col
        , val == '*'
      ]
