{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiWayIf   #-}
module Cards where
import           Control.Monad.State.Lazy (MonadState (get, put), State,
                                           evalState)
import           Data.Char                (intToDigit, toLower)
import           Text.Printf              (printf)
import           Text.Read                (readMaybe)

type DistributivePunchCard = NumberPunchCard
type NumericPunchCard = NumberPunchCard
type NumberPunchCard = PunchCard

type OperationPunchCard = PunchCard

type PunchCard = [PunchCardLine]
type PunchCardLine = String

processOperation :: (MathsOperation -> b) -> (OutputOperation -> b) -> (a -> b) -> ProcessOperation a -> b
processOperation f1 f2 f3 e = case e of
  Math a     -> f1 a
  Output a   -> f2 a
  Variable a -> f3 a

instance Functor ProcessOperation where
  fmap :: (a -> b) -> ProcessOperation a  -> ProcessOperation b
  fmap f e = case e of
    Math m     -> Math m
    Output o   -> Output o
    Variable v -> Variable (f v)


type UnboundOperation = ProcessOperation UnboundVariableOperation
type PairedOperation = ProcessOperation (Int, UnboundVariableOperation)

data ProcessOperation a = Math MathsOperation | Output OutputOperation | Variable a
  deriving Show

type Operation = ProcessOperation VariableOperation

data MathsOperation = Addition | Subtraction | Multiply | Divide
  deriving Show
data OutputOperation = Print | Bell
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

bindOperations :: [Int] -> [UnboundOperation] -> [Operation]
bindOperations vs us = operations
  where
  -- this is lefties alone and righties as data with variable
  operations :: [Operation]
  operations = fmap bindVariableOperation <$> evalState (mapM pairVars us) vs
    where
      pairVars :: UnboundOperation -> State [Int] PairedOperation
      pairVars (Variable r) =
        get >>= \vars -> case vars of
            []   -> error "ran out of vars :("
            v:vs -> return (vs, Variable (v, r))
          >>= \(v, e) -> put v >> return e
      pairVars (Math l) =
        get >>= \vars -> (\(v, e) -> put v >> return e) (vars, Math l)
      pairVars (Output m) =
        get >>= \vars -> (\(v, e) -> put v >> return e) (vars, Output m)

      bindVariableOperation :: (Int, UnboundVariableOperation) -> VariableOperation
      bindVariableOperation (n, UnboundSupplyRetaining) = SupplyRetaining n
      bindVariableOperation (n, UnboundSupplyZeroing)   = SupplyZeroing n
      bindVariableOperation (n, UnboundStore)           = Store n
      bindVariableOperation (n, UnboundStorePrimed)     = StorePrimed n
      bindVariableOperation (n, UnboundForwards)        = Forwards n
      bindVariableOperation (n, UnboundBackwards)       = Backwards n
      bindVariableOperation (n, UnboundForwardsCond)    = ForwardsCond n
      bindVariableOperation (n, UnboundBackwardsCond)   = BackwardsCond n

parseOperation :: OperationPunchCard -> UnboundOperation
parseOperation card =
  if  | card == addition      -> Math Addition
      | card == subtraction   -> Math Subtraction
      | card == multiply      -> Math Multiply
      | card == divide        -> Math Divide
      | card == loadPreserve  -> Variable UnboundSupplyRetaining
      | card == loadZero      -> Variable UnboundSupplyZeroing
      | card == store         -> Variable UnboundStore
      | card == storePrimed   -> Variable UnboundStorePrimed
      | card == forwards      -> Variable UnboundForwards
      | card == backwards     -> Variable UnboundBackwards
      | card == forwardsCond  -> Variable UnboundForwards
      | card == backwardsCond -> Variable UnboundBackwards
      | True                  -> error $ "unknown operation card: " ++ show card

parseStringToOperation :: String -> Maybe OperationPunchCard
parseStringToOperation s = case toLower <$> s of
  "addition"      -> Just addition
  "subtraction"   -> Just subtraction
  "multiply"      -> Just multiply
  "divide"        -> Just divide
  "loadPreserve"  -> Just loadPreserve
  "loadZero"      -> Just  loadZero
  "store"         -> Just store
  "storePrimed"   -> Just storePrimed
  "forwards"      -> Just forwards
  "backwards"     -> Just backwards
  "forwardsCond"  -> Just forwardsCond
  "backwardsCond" -> Just backwardsCond
  _               -> Nothing

parseStringToVariable :: String -> Maybe NumberPunchCard
parseStringToVariable a =
  let num = readMaybe a in
  case num of
    Nothing -> Nothing
    Just num ->
      if num > 999 then Nothing
      else Just $ transpose $ parseCharToNumCard <$> a

parseStringToParameter :: String -> Maybe NumericPunchCard
parseStringToParameter a =
  let num = readMaybe a in
  case num of
    Nothing -> Nothing
    Just num ->
      if
        | num > 99999999999999999999999999999999999999999999999999 -> Nothing
        | num < -99999999999999999999999999999999999999999999999999 -> Nothing
        | num >= 0 -> Just $ transpose $ positive : (parseCharToNumCard <$> printf "%050s" a)
        | num < 0  -> Just $ transpose $ negative : (parseCharToNumCard <$> (tail . printf "%050s") a)
        | True -> Nothing
      where
        positive :: [Char]
        positive = [' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ']
        negative :: [Char]
        negative = ['*', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ']
        

transpose :: [[Char]] -> [[Char]]
transpose ((a:b:c:d:e:f:g:h:i:j:_) : rest) = 
  let (a1:b1:c1:d1:e1:f1:g1:h1:i1:j1:_) = transpose rest in
    [a:a1, b:b1, c:c1, d:d1, e:e1, f:f1, g:g1, h:h1, i:i1, j:j1]
transpose _ = [[],[],[],[],[],[],[],[],[],[]]

parseCharToNumCard :: Char -> [Char]
parseCharToNumCard s = case s of
  '0' -> ['*', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ']
  '1' -> [' ', '*', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ']
  '2' -> [' ', ' ', '*', ' ', ' ', ' ', ' ', ' ', ' ', ' ']
  '3' -> [' ', ' ', ' ', '*', ' ', ' ', ' ', ' ', ' ', ' ']
  '4' -> [' ', ' ', ' ', ' ', '*', ' ', ' ', ' ', ' ', ' ']
  '5' -> [' ', ' ', ' ', ' ', ' ', '*', ' ', ' ', ' ', ' ']
  '6' -> [' ', ' ', ' ', ' ', ' ', ' ', '*', ' ', ' ', ' ']
  '7' -> [' ', ' ', ' ', ' ', ' ', ' ', ' ', '*', ' ', ' ']
  '8' -> [' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', '*', ' ']
  '9' -> [' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', '*']
  _   -> error "unreachable code?"


addition :: OperationPunchCard
addition      = ["* ", "  ", "  ", "  ", "  "]
subtraction :: OperationPunchCard
subtraction   = [" *", "  ", "  ", "  ", "  "]
multiply :: OperationPunchCard
multiply      = ["  ", "* ", "  ", "  ", "  "]
divide :: OperationPunchCard
divide        = ["  ", " *", "  ", "  ", "  "]
loadPreserve :: OperationPunchCard
loadPreserve  = ["  ", "  ", "* ", "  ", "  "]
loadZero :: OperationPunchCard
loadZero      = ["  ", "  ", " *", "  ", "  "]
store :: OperationPunchCard
store         = ["  ", "  ", "  ", "* ", "  "]
storePrimed :: OperationPunchCard
storePrimed   = ["  ", "  ", "  ", " *", "  "]
forwards :: OperationPunchCard
forwards      = ["* ", "  ", "  ", "  ", "* "]
backwards :: OperationPunchCard
backwards     = [" *", "  ", "  ", "  ", "* "]
forwardsCond :: OperationPunchCard
forwardsCond  = ["* ", "  ", "  ", "  ", " *"]
backwardsCond :: OperationPunchCard
backwardsCond = [" *", "  ", "  ", "  ", " *"]

parseVariable :: DistributivePunchCard -> Int
parseVariable = read . parseCardNumber

parseNumeric :: NumericPunchCard -> Integer
parseNumeric []   = error "empty punchcard"
    -- remove the first char from each line of the card
parseNumeric card = read $ parseSign card : parseCardNumber (tail <$> card) -- this removes the leading space in numeric cards
    where
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
