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

data DistributiveRecord = MkOp {
  cons :: UnboundOperation,
  card :: DistributivePunchCard
}

type PunchCard = [PunchCardLine]
type PunchCardLine = String

processOperation :: (MathsOperation -> b) -> (OutputOperation -> b) -> (a -> b) -> ProcessOperation a -> b
processOperation f1 f2 f3 e = case e of
  Math a     -> f1 a
  Output a   -> f2 a
  Variable a -> f3 a

showOp :: Show a => ProcessOperation a -> String
showOp = processOperation show show show

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
instance Show MathsOperation where
  show :: MathsOperation -> String
  show Addition    = "addition"
  show Subtraction = "subtraction"
  show Multiply    = "multiplication"
  show Divide      = "division"

data OutputOperation = Print | Bell | Halt
instance Show OutputOperation where
  show :: OutputOperation -> String
  show Print = "print"
  show Bell  = "bell"
  show Halt  = "halt"

data UnboundVariableOperation =
  UnboundSupplyRetaining
  | UnboundSupplyZeroing
  | UnboundStore
  | UnboundStorePrimed
  | UnboundForwards
  | UnboundForwardsCond
  | UnboundBackwards
  | UnboundBackwardsCond
  deriving Enum

instance Show UnboundVariableOperation where
  show :: UnboundVariableOperation -> String
  show UnboundSupplyRetaining = "loadPreserve"
  show UnboundSupplyZeroing   = "loadZero"
  show UnboundStore           = "store"
  show UnboundStorePrimed     = "storePrimed"
  show UnboundForwards        = "forwards"
  show UnboundBackwards       = "backwards"
  show UnboundForwardsCond    = "forwardsCond"
  show UnboundBackwardsCond   = "backwardsCond"

data VariableOperation =
  SupplyRetaining Int
  | SupplyZeroing Int
  | Store Int
  | StorePrimed Int
  | Forwards Int
  | ForwardsCond Int
  | Backwards Int
  | BackwardsCond Int
  deriving (Show)

toValEnum :: (Eq a, Num a) => a -> Int -> VariableOperation
toValEnum num v = case num of
  0 -> SupplyRetaining v
  1 -> SupplyZeroing v
  2 -> Store v
  3 -> StorePrimed v
  4 -> Forwards v
  5 -> ForwardsCond v
  6 -> Backwards v
  7 -> BackwardsCond v
  _ -> error ""


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
      bindVariableOperation (n, o) = toValEnum (fromEnum o) n

parseOperation :: OperationPunchCard -> UnboundOperation
parseOperation c =
    let matchingOps = dropWhile (\o -> c /= card o) operations in
    case matchingOps of
      []    -> error $ "unknown operation card: " ++ show c
      (o:_) -> cons o

parseStringToOperation :: String -> Maybe OperationPunchCard
parseStringToOperation s =
  let matchingOps = dropWhile (\o -> lower s /= (lower . showOp . cons) o) operations in
  case matchingOps of
    []    -> Nothing
    (o:_) -> Just $ card o
  where
    lower :: (Functor f) => f Char -> f Char
    lower = fmap toLower

parseOperationToString :: OperationPunchCard -> Maybe String
parseOperationToString s =
  let matchingOps = dropWhile (\o -> s /= card o) operations in
    case matchingOps of
      []    -> Nothing
      (o:_) -> Just . showOp . cons $ o

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
transpose ([a, b, c, d, e, f, g, h, i, j] : rest) =
  let [a', b', c', d', e', f', g', h', i', j'] = transpose rest in
    [a:a', b:b', c:c', d:d', e:e', f:f', g:g', h:h', i:i', j:j']
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
isDistributive a =
  let dists = filter (\o -> case cons o of
        Variable _ -> True
        _          -> False) operations in
  let matchingOps = dropWhile (\o -> a /= card o) dists in
  case matchingOps of
    [] -> False
    _  -> True

operations :: [DistributiveRecord]
operations      = [
  MkOp (Math Addition) ["* ", "  ", "  ", "  ", "  "],
  MkOp (Math Subtraction) [" *", "  ", "  ", "  ", "  "],
  MkOp (Math Multiply) ["  ", "* ", "  ", "  ", "  "],
  MkOp (Math Divide) ["  ", " *", "  ", "  ", "  "],
  MkOp (Output Print) ["  ", "  ", "  ", " *", " *"],
  MkOp (Output Bell) ["  ", "  ", "  ", "* ", "* "],
  MkOp (Output Halt) ["  ", "  ", "  ", "**", "**"],
  MkOp (Variable UnboundSupplyRetaining) ["  ", "  ", "* ", "  ", "  "],
  MkOp (Variable UnboundSupplyZeroing) ["  ", "  ", " *", "  ", "  "],
  MkOp (Variable UnboundStore) ["  ", "  ", "  ", "* ", "  "],
  MkOp (Variable UnboundStorePrimed) ["  ", "  ", "  ", " *", "  "],
  MkOp (Variable UnboundForwards) ["* ", "  ", "  ", "  ", "* "],
  MkOp (Variable UnboundBackwards) [" *", "  ", "  ", "  ", "* "],
  MkOp (Variable UnboundForwardsCond) ["* ", "  ", "  ", "  ", " *"],
  MkOp (Variable UnboundBackwardsCond) [" *", "  ", "  ", "  ", " *"]
  ]

parseVariable :: DistributivePunchCard -> Int
parseVariable = read . parseCardNumber

parseNumeric :: NumericPunchCard -> Integer
parseNumeric []   = error "empty punchcard"
    -- remove the first char from each line of the card
parseNumeric c = read $ parseSign c : parseCardNumber (tail <$> c) -- this removes the leading space in numeric cards
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
