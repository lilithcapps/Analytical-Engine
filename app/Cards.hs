{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE InstanceSigs       #-}
{-# LANGUAGE MultiWayIf         #-}
module Cards where
import           Control.Monad.State.Lazy (MonadState (get, put), State,
                                           evalState)
import           Data.Char                (intToDigit, toLower)
import           Data.Data                (Data)
import           Text.Printf              (printf)
import           Text.Read                (readMaybe)

type DistributivePunchCard = NumberPunchCard
type NumericPunchCard = NumberPunchCard
type NumberPunchCard = PunchCard

type OperationPunchCard = PunchCard

data DistributiveRecord = MkOp {
  name :: String,
  card :: DistributivePunchCard
}

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
  deriving (Show, Data)
data OutputOperation = Print | Bell
  deriving (Show, Data)

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
  deriving (Show, Data)

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
parseOperation c
    | c == card addition      = Math Addition
    | c == card subtraction   = Math Subtraction
    | c == card multiply      = Math Multiply
    | c == card divide        = Math Divide
    | c == card bell          = Output Bell
    | c == card write         = Output Print
    | c == card loadPreserve  = Variable UnboundSupplyRetaining
    | c == card loadZero      = Variable UnboundSupplyZeroing
    | c == card store         = Variable UnboundStore
    | c == card storePrimed   = Variable UnboundStorePrimed
    | c == card forwards      = Variable UnboundForwards
    | c == card backwards     = Variable UnboundBackwards
    | c == card forwardsCond  = Variable UnboundForwards
    | c == card backwardsCond = Variable UnboundBackwards
    | otherwise             = error $ "unknown operation card: " ++ show c

parseStringToOperation :: String -> Maybe OperationPunchCard
parseStringToOperation s
  | lower s == (lower . name) addition      = Just . card $ addition
  | lower s == (lower . name) subtraction   = Just . card $ subtraction
  | lower s == (lower . name) multiply      = Just . card $ multiply
  | lower s == (lower . name) divide        = Just . card $ divide
  | lower s == (lower . name) bell          = Just . card $ bell
  | lower s == (lower . name) write         = Just . card $ write
  | lower s == (lower . name) loadPreserve  = Just . card $ loadPreserve
  | lower s == (lower . name) loadZero      = Just . card $ loadZero
  | lower s == (lower . name) store         = Just . card $ store
  | lower s == (lower . name) storePrimed   = Just . card $ storePrimed
  | lower s == (lower . name) forwards      = Just . card $ forwards
  | lower s == (lower . name) backwards     = Just . card $ backwards
  | lower s == (lower . name) forwardsCond  = Just . card $ forwardsCond
  | lower s == (lower . name) backwardsCond = Just . card $ backwardsCond
  | otherwise                               = Nothing
  where
    lower :: (Functor f) => f Char -> f Char
    lower = fmap toLower

parseOperationToString :: OperationPunchCard -> Maybe String
parseOperationToString s
  | s == card addition      = Just . name $ addition
  | s == card subtraction   = Just . name $ subtraction
  | s == card multiply      = Just . name $ multiply
  | s == card divide        = Just . name $ divide
  | s == card bell          = Just . name $ bell
  | s == card write         = Just . name $ write
  | s == card loadPreserve  = Just . name $ loadPreserve
  | s == card loadZero      = Just . name $ loadZero
  | s == card store         = Just . name $ store
  | s == card storePrimed   = Just . name $ storePrimed
  | s == card forwards      = Just . name $ forwards
  | s == card backwards     = Just . name $ backwards
  | s == card forwardsCond  = Just . name $ forwardsCond
  | s == card backwardsCond = Just . name $ backwardsCond
  | otherwise               = Nothing

parseStringToVariable :: String -> Maybe NumberPunchCard
parseStringToVariable a =
  let num = readMaybe a in
  case num of
    Nothing -> Nothing
    Just n ->
      if n > (999 :: Integer) then Nothing
      else Just $ transpose $ parseCharToNumCard <$> printf "%03s" a

parseStringToParameter :: String -> Maybe NumericPunchCard
parseStringToParameter a =
  let num = readMaybe a :: Maybe Integer in
  case num of
    Nothing -> Nothing
    Just n ->
      if
        | n > 99999999999999999999999999999999999999999999999999 -> Nothing
        | n < -99999999999999999999999999999999999999999999999999 -> Nothing
        | n >= 0 -> Just $ transpose $ positive : (parseCharToNumCard <$> printf "%050s" a)
        | n < 0  -> Just $ transpose $ negative : (parseCharToNumCard <$> (tail . printf "%050s") a)
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

isDistributive :: OperationPunchCard -> Bool
isDistributive a
  | a == card loadPreserve = True
  | a == card loadZero = True
  | a == card store = True
  | a == card storePrimed = True
  | a == card forwards = True
  | a == card backwards = True
  | a == card forwardsCond = True
  | a == card backwardsCond = True
  | otherwise = False

addition      :: DistributiveRecord
subtraction   :: DistributiveRecord
multiply      :: DistributiveRecord
divide        :: DistributiveRecord
write         :: DistributiveRecord
bell          :: DistributiveRecord
loadPreserve  :: DistributiveRecord
loadZero      :: DistributiveRecord
store         :: DistributiveRecord
storePrimed   :: DistributiveRecord
forwards      :: DistributiveRecord
backwards     :: DistributiveRecord
forwardsCond  :: DistributiveRecord
backwardsCond :: DistributiveRecord
addition      = MkOp "addition" ["* ", "  ", "  ", "  ", "  "]
subtraction   = MkOp "subtraction" [" *", "  ", "  ", "  ", "  "]
multiply      = MkOp "multiplication" ["  ", "* ", "  ", "  ", "  "]
divide        = MkOp "division" ["  ", " *", "  ", "  ", "  "]
write         = MkOp "print" ["  ", "  ", "  ", " *", " *"]
bell          = MkOp "bell" ["  ", "  ", "  ", "* ", "* "]
loadPreserve  = MkOp "loadPreserve" ["  ", "  ", "* ", "  ", "  "]
loadZero      = MkOp "loadZero" ["  ", "  ", " *", "  ", "  "]
store         = MkOp "store" ["  ", "  ", "  ", "* ", "  "]
storePrimed   = MkOp "storePrimed" ["  ", "  ", "  ", " *", "  "]
forwards      = MkOp "forwards" ["* ", "  ", "  ", "  ", "* "]
backwards     = MkOp "backwards" [" *", "  ", "  ", "  ", "* "]
forwardsCond  = MkOp "forwardsCond" ["* ", "  ", "  ", "  ", " *"]
backwardsCond = MkOp "backwardsCond" [" *", "  ", "  ", "  ", " *"]

parseVariable :: DistributivePunchCard -> Int
parseVariable = read . parseCardNumber

parseNumeric :: NumericPunchCard -> Integer
parseNumeric []   = error "empty punchcard"
    -- remove the first char from each line of the card
parseNumeric c = read $ (parseSign c) : parseCardNumber (tail <$> c) -- this removes the leading space in numeric cards
    where
    parseSign [] = error "empty punchcard"
    parseSign c = case head . head $ c of
        '*' -> '-'
        ' ' -> ' ' 
        _   -> error $ "badly formatted punchcard: " ++ show c

parseCardNumber :: NumberPunchCard -> String
parseCardNumber [] = error "empty punchcard"
parseCardNumber cardLines =
    [ intToDigit col
    | row <- [0..(length . head) cardLines - 1]
    , col <- [0..length cardLines - 1]
    , let val = cardLines !! col !! row
    , val == '*'
    ]
