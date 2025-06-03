{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

import           Control.Lens             (ix, (&), (.~))
import           Control.Monad.State.Lazy
import           Data.Char                (intToDigit)
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
type Primed = Bool
type RunUpLever = Bool

type MinecartM = Maybe (Minecart Operation [Operation] [Operation])

type EngineState = (MinecartM, MillValue, MathsOperation, StoreValue, Primed, RunUpLever)

data MathsOperation = Add | Subtract | Multiply | Divide
  deriving Show
data UnboundVariableOperation =
  UnboundSupplyRetaining
  | UnboundSupplyZeroing
  | UnboundStore
  | UnboundStorePrimed
  | UnboundForwards
  | UnboundForwardsCond
  | UnboundBackwards
  | UnboundBackwardsCond
  deriving Show
data VariableOperation =
  SupplyRetaining Int
  | SupplyZeroing Int
  | Store Int
  | StorePrimed Int
  | Forwards Int
  | ForwardsCond Int
  | Backwards Int
  | BackwardsCond Int
  deriving Show

applyParameters :: [Integer] -> [Operation] -> [StoreValue]
applyParameters params ops = states
  where
    store = params ++ [0,0..] :: StoreValue
    mill = (0, 0) :: MillValue

    states :: [StoreValue]
    states = getStoreValue <$> evaluateState (mount ops, mill, Add, store, False, False)
      where
        getStoreValue :: EngineState -> StoreValue
        getStoreValue (_, _, _, s, _, _) = s

        getOperation :: EngineState -> Maybe Operation
        getOperation (mc, _, _, _, _, _) = mc >>= dismount

        evaluateState :: EngineState -> [EngineState]
        evaluateState s =
          let state =
                (doMovement . doOperation (getOperation s) $ s) : go state
          in state
          where
            go :: [EngineState] -> [EngineState]
            go ((Nothing, _, _, _, _, _):_) = []
            go (s:_) =
              let newState =
                    (doMovement . doOperation (getOperation s) $ s) : go newState
              in newState
            go [] = []

            doMovement :: EngineState -> EngineState
            doMovement (mc, mill, op, store, primed, lever) =
              let newMc = (case mc >>= dismount of
                    Just (Right (Forwards n))  -> mc >>= next >>= forwardsN n
                    Just (Right (Backwards n)) -> mc >>= backwardsN (n-1)
                    Just (Right (ForwardsCond n)) -> mc >>= if lever then next else next >=> forwardsN n
                    Just (Right (BackwardsCond n)) -> mc >>= if lever then next else backwardsN (n-1)
                    Nothing                    -> Nothing
                    _                          -> mc >>= next)
              in (newMc, mill, op, store, primed, lever)

            doOperation :: Maybe Operation -> EngineState -> EngineState
            doOperation Nothing s = s
            doOperation (Just op) s@(mc, (a, b), _, store, primed, lever) =
              case (op, primed) of
              (Right op@(SupplyRetaining _), True) -> doArithmetic . doDistributive op $ s
              (Right op@(SupplyZeroing _), True) -> doArithmetic . doDistributive op $ s
              (Right r, _) -> doDistributive r s
              (Left l, _) -> (mc, (a, b), l, store, primed, lever)
              where
                doDistributive :: VariableOperation -> EngineState -> EngineState
                doDistributive op s@(mc, (a, b), mathOp, store, primed, lever) = case op of
                  SupplyRetaining n -> if primed then (mc, (store !! n, b), mathOp, store, not primed, lever)
                                                else (mc, (a, store !! n), mathOp, store, not primed, lever)
                  SupplyZeroing n   -> if primed then (mc, (store !! n, b), mathOp, store & ix n .~ 0, not primed, lever)
                                                else (mc, (a, store !! n), mathOp, store & ix n .~ 0, not primed, lever)
                  Store n           -> (mc, (0, b), mathOp, store & ix n .~ a, primed, lever)
                  StorePrimed n     -> (mc, (a, 0), mathOp, store & ix n .~ b, primed, lever)
                  _                 -> s

                doArithmetic :: EngineState -> EngineState
                doArithmetic (mc, (a, b), op, store, primed, lev) =
                  let (mill, lever) = case op of
                        Add      -> ((a + b, 0), lev)
                        Subtract -> ((a - b, 0), (fst mill < 0) || lev)
                        Multiply -> ((a * b, 0), lever)
                        Divide   -> ((a `div` b, a `mod` b), lev)
                  in (mc, mill, op, store, primed, lev)

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
      bindVariableOperation (n, UnboundForwardsCond)    = ForwardsCond n
      bindVariableOperation (n, UnboundBackwardsCond)   = BackwardsCond n


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
      if | card == addCard            -> Left Add
          | card == subtractCard      -> Left Subtract
          | card == multiplyCard      -> Left Multiply
          | card == divideCard        -> Left Divide
          | card == loadPreserveCard  -> Right UnboundSupplyRetaining
          | card == loadZeroCard      -> Right UnboundSupplyZeroing
          | card == storeCard         -> Right UnboundStore
          | card == storePrimedCard   -> Right UnboundStorePrimed
          | card == forwardsCard      -> Right UnboundForwards
          | card == backwardsCard     -> Right UnboundBackwards
          | card == forwardsCondCard  -> Right UnboundForwards
          | card == backwardsCondCard -> Right UnboundBackwards
          | True                      -> error $ "unknown operation card: " ++ show card
      where
        addCard            = ["* ", "  ", "  ", "  ", "  "]
        subtractCard       = [" *", "  ", "  ", "  ", "  "]
        multiplyCard       = ["  ", "* ", "  ", "  ", "  "]
        divideCard         = ["  ", " *", "  ", "  ", "  "]
        loadPreserveCard   = ["  ", "  ", "* ", "  ", "  "]
        loadZeroCard       = ["  ", "  ", " *", "  ", "  "]
        storeCard          = ["  ", "  ", "  ", "* ", "  "]
        storePrimedCard    = ["  ", "  ", "  ", " *", "  "]
        forwardsCard       = ["* ", "  ", "  ", "  ", "* "]
        backwardsCard      = [" *", "  ", "  ", "  ", "* "]
        forwardsCondCard   = ["* ", "  ", "  ", "  ", " *"]
        backwardsCondCard  = [" *", "  ", "  ", "  ", " *"]

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
      _   -> error $ "badly formatted punchcard: " ++ show card

    parseCardNumber :: NumberPunchCard -> String
    parseCardNumber [] = error "empty punchcard"
    parseCardNumber cardLines =
      [ intToDigit (col + 1)
        | row <- [0..length . head $ cardLines]
        , col <- [0..length cardLines]
        , let val = cardLines !! row !! col
        , val == '*'
      ]
