{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}
{-# LANGUAGE InstanceSigs      #-}

import           Control.Lens             (ix, (&), (.~))
import           Control.Monad.State.Lazy
import           Data.Char                (intToDigit)
import qualified Data.Text.Lazy           as T
import qualified Data.Text.Lazy.IO        as IO
import           Minecart                 (Minecart, backwardsN, dismount,
                                           forwardsN, mount, next)
import           Prelude                  hiding (Left, Right)

type DistributivePunchCard = NumberPunchCard
type NumericPunchCard = NumberPunchCard
type NumberPunchCard = PunchCard

type OperationPunchCard = PunchCard

type PunchCard = [PunchCardLine]
type PunchCardLine = String

data Either3 a b c = Left a | Middle b | Right c
either3 :: (a -> d) -> (b -> d) -> (c -> d) -> Either3 a b c -> d
either3 f1 f2 f3 e = case e of
  Left e   -> f1 e
  Middle e -> f2 e
  Right e  -> f3 e

instance Functor (Either3 a b) where
  fmap :: (c -> d) -> Either3 a b c -> Either3 a b d
  fmap f e = case e of
    Left l   -> Left l
    Middle m -> Middle m
    Right r  -> Right (f r)

either3ToOperation :: Either3 MathsOperation OutputOperation VariableOperation -> Operation
either3ToOperation e = case e of
  Left e   -> Math e
  Middle e -> Output e
  Right e  -> Variable e


type UnboundOperation = Either3 MathsOperation OutputOperation UnboundVariableOperation
type PairedOperation = Either3 MathsOperation OutputOperation (Int, UnboundVariableOperation)

data Operation = Math MathsOperation | Output OutputOperation | Variable VariableOperation
  deriving Show

type MinecartM = Maybe (Minecart Operation [Operation] [Operation])
type IngressAxis = ((Integer, Integer), Integer)
type EgressAxis = (Integer, Integer)
type StoreValue = [Integer]
type TakingFirst = Bool
type RunUpLever = Bool

data EngineState = MkState {
  minecart  :: MinecartM,
  ingress   :: IngressAxis,
  egress    :: EgressAxis,
  operation :: MathsOperation,
  store     :: StoreValue,
  first     :: TakingFirst,
  lever     :: RunUpLever,
  output    :: OutputValue
}

data MathsOperation = Add | Subtract | Multiply | Divide
  deriving Show
data OutputOperation = Print | Bell
  deriving Show
data OutputValue = PrintV EgressAxis | RingBell | None
  deriving (Show, Eq)

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

applyParameters :: [Integer] -> [Operation] -> [OutputValue]
applyParameters params ops = filter (/= None) states
  where
    str = params ++ [0,0..] :: StoreValue
    iAxis = ((0, 0), 0) :: IngressAxis
    eAxis = (0, 0) :: EgressAxis

    states :: [OutputValue]
    states = output <$> evaluateState (MkState (mount ops) iAxis eAxis Add str False False None)
      where
        getOperation :: EngineState -> Maybe Operation
        getOperation = dismount <=< minecart

        evaluateState :: EngineState -> [EngineState]
        evaluateState s =
          let state = (doMovement . doOperation $ s) : go state
          in state
          where
            go :: [EngineState] -> [EngineState]
            go ((MkState {minecart = Nothing}):_) = []
            go (s:_) =
              let newState = (doMovement . doOperation $ s { output = None }) : go newState
              in newState
            go [] = []

            doMovement :: EngineState -> EngineState
            doMovement s =
              let mc = minecart s in
              let newMc = (case minecart s >>= dismount of
                    Just (Variable (Forwards n))  -> mc >>= next >>= forwardsN n
                    Just (Variable (Backwards n)) -> mc >>= backwardsN (n-1)
                    Just (Variable (ForwardsCond n)) -> mc >>= if lever s then next else next >=> forwardsN n
                    Just (Variable (BackwardsCond n)) -> mc >>= if lever s then next else backwardsN (n-1)
                    Nothing                    -> Nothing
                    _                          -> mc >>= next)
              in s { minecart = newMc }

            doOperation :: EngineState -> EngineState
            doOperation s =
              case getOperation s of
                Nothing -> s
                Just op -> case (op, first s) of
                  (Variable op@(SupplyRetaining _), True) -> doArithmetic . doDistributive op $ s
                  (Variable op@(SupplyZeroing _), True) -> doArithmetic . doDistributive op $ s
                  (Variable r, _) -> doDistributive r s
                  (Output Print, _) -> s { output = PrintV (egress s) }
                  (Output Bell, _) -> s { output = RingBell }
                  (Math l, _) -> s { operation = l }
              where
                doDistributive :: VariableOperation -> EngineState -> EngineState
                doDistributive op s@(MkState { ingress = (ing@(_, a'), b), egress = (ex, ex'), store = str, first = fst }) = case op of
                  SupplyRetaining n -> if first s then s { first = not fst, ingress = ((str !! n, a'), b) }
                                                   else s { first = not fst, ingress = (ing, str !! n) }
                  SupplyZeroing n   -> if first s then s { first = not fst, ingress = ((str !! n, a'), b), store = str & ix n .~ 0 }
                                                   else s { first = not fst, ingress = (ing, str !! n), store = str & ix n .~ 0 }
                  Store n           -> s { store = str & ix n .~ ex}
                  StorePrimed n     -> s { store = str & ix n .~ ex'}
                  _                 -> s

                doArithmetic :: EngineState -> EngineState
                doArithmetic s@(MkState { ingress = ((a, a'), b), egress = (_, ex')}) =
                  let (eAxis, lev) = case operation s of
                        Add      -> ((a + b, ex'), lever s)
                        Subtract -> ((a - b, ex'), (fst eAxis < 0) || lever s)
                        Multiply -> ((a * b, ex'), lever s)
                        Divide   -> ((a `div` b, a `mod` b), lever s)
                  in s { egress = eAxis, lever = lev }

bindOperations :: [Int] -> [UnboundOperation] -> [Operation]
bindOperations vs us = operations
  where
  -- this is lefties alone and righties as data with variable
  operations :: [Operation]
  operations = either3ToOperation . fmap bindVariableOperation <$> evalState (mapM pairVars us) vs
    where
      pairVars :: UnboundOperation -> State [Int] PairedOperation
      pairVars (Right r) =
        get >>= \vars -> case vars of
            []   -> error "ran out of vars :("
            v:vs -> return (vs, Right (v, r))
          >>= \(v, e) -> put v >> return e
      pairVars (Left l) =
        get >>= \vars -> (\(v, e) -> put v >> return e) (vars, Left l)
      pairVars (Middle m) =
        get >>= \vars -> (\(v, e) -> put v >> return e) (vars, Middle m)

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
        Middle Print, Middle Bell,
        Right UnboundStore,
        Right UnboundSupplyZeroing,
        Right UnboundSupplyZeroing,
        Middle Print, Middle Bell,
        Right UnboundStore
        ]
  let inputVars = [0, 1, 5, 1, 5, 6]
  let inputParams = [5, 10]

  let opChain = bindOperations inputVars inputOps
  let computedValues = applyParameters inputParams opChain

  putStrLn . (++) "Initial Operations: " $ show . prettyPrintEither $ inputOps
  putStrLn $ "Initial Variables: " ++ show inputVars
  putStrLn $ "Initial Parameters: " ++ show inputParams

  print . take 20 $ opChain
  mapM_ processOutput computedValues

  where
    processOutput :: OutputValue -> IO ()
    processOutput (PrintV a) = print a
    processOutput RingBell   = putChar '\a' >> putStrLn "Operator Attention - Press enter to resume analysis: " >> getLine >> return ()
    processOutput None       = return ()

    readCard :: FilePath -> IO T.Text
    readCard = IO.readFile

    prettyPrintEither :: (Functor f, Show a, Show b, Show c) => f (Either3 a b c) -> f String
    prettyPrintEither = (either3 show show show <$>)

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
