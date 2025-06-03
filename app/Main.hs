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
type RunUpLeverActive = Bool -- For clarity, though it's just Bool

type MinecartM = Maybe (Minecart Operation [Operation] [Operation])

type EngineState = (MinecartM, MillValue, MathsOperation, StoreValue, Primacy, RunUpLeverActive) -- Added runUpLeverActive

data MathsOperation = Add | Subtract | Multiply | Divide
  deriving Show
data UnboundVariableOperation =
  UnboundSupplyRetaining
  | UnboundSupplyZeroing
  | UnboundStore
  | UnboundStorePrimed
  | UnboundForwards
  | UnboundBackwards
  | UnboundConditionalForwards
  | UnboundConditionalBackwards
  | UnboundSetRunUpLever -- New
  | UnboundClearRunUpLever -- New
  deriving Show
data VariableOperation =
  SupplyRetaining Int
  | SupplyZeroing Int
  | Store Int
  | StorePrimed Int
  | Forwards Int
  | Backwards Int
  | ConditionalForwards Int
  | ConditionalBackwards Int
  | SetRunUpLever Int -- New, consumes dummy variable
  | ClearRunUpLever Int -- New, consumes dummy variable
  deriving Show

applyParameters :: [Integer] -> [Operation] -> [StoreValue]
applyParameters params ops = states
  where
    store = params ++ [0,0..] :: StoreValue
    mill = (0, 0) :: MillValue

    states :: [StoreValue]
    states = evalState (mapM engine ops) (mount ops, mill, Add, store, False, False) -- Added False for runUpLeverActive
      where
        engine :: Operation -> State EngineState StoreValue
        engine op = get
          >>= put . doOperation op
          >> get
          >>= (\(mc, millVal, mathOp, storeVal, primVal, rUpVal) -> return (doMovement mc rUpVal, millVal, mathOp, storeVal, primVal, rUpVal))
          >>= (\(_, _, _, s, _, _) -> return s) -- Adjusted for new state tuple

        doMovement :: MinecartM -> RunUpLeverActive -> MinecartM
        doMovement mc runUpLeverActive = case mc >>= dismount of
          Just (Right (Forwards n))  -> mc >>= next >>= forwardsN n
          Just (Right (Backwards n)) -> mc >>= backwardsN (n-1)
          Just (Right (ConditionalForwards n)) ->
            if runUpLeverActive then mc >>= next >>= forwardsN n
            else mc >>= next
          Just (Right (ConditionalBackwards n)) ->
            if runUpLeverActive then mc >>= backwardsN (n-1)
            else mc >>= next
          Nothing                    -> Nothing
          _                          -> mc >>= next -- Default for MathOperations or other unhandled VarOps

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
    doOperation op s@(mc, millVal@(millA, millB), mathOp, storeVal, primVal, rUpVal) = case (op, primVal) of
      -- Handle Set/Clear RunUpLever first as they are independent of primVal and mill operations
      (Right (SetRunUpLever _), _)    -> (mc, millVal, mathOp, storeVal, primVal, True) -- Set runUpLeverActive to True
      (Right (ClearRunUpLever _), _)  -> (mc, millVal, mathOp, storeVal, primVal, False) -- Set runUpLeverActive to False

      (Right op'@(SupplyRetaining _), True) -> doArithmetic . doDistributive op' $ s
      (Right op'@(SupplyZeroing _), True)   -> doArithmetic . doDistributive op' $ s
      (Right r, _)                         -> doDistributive r s
      (Left l, _)                          -> (mc, millVal, l, storeVal, primVal, rUpVal) -- Keep rUpVal
      where
        doDistributive :: VariableOperation -> EngineState -> EngineState
        doDistributive op s'@(mc', (a, b), currentMathOp, currentStore, currentPrimed, currentRUp) = case op of
          SupplyRetaining n -> if currentPrimed then (mc', (currentStore !! n, b), currentMathOp, currentStore, not currentPrimed, currentRUp)
                                              else (mc', (a, currentStore !! n), currentMathOp, currentStore, not currentPrimed, currentRUp)
          SupplyZeroing n   -> if currentPrimed then (mc', (currentStore !! n, b), currentMathOp, currentStore & ix n .~ 0, not currentPrimed, currentRUp)
                                              else (mc', (a, currentStore !! n), currentMathOp, currentStore & ix n .~ 0, not currentPrimed, currentRUp)
          Store n           -> (mc', (0, b), currentMathOp, currentStore & ix n .~ a, currentPrimed, currentRUp)
          StorePrimed n     -> (mc', (a, 0), currentMathOp, currentStore & ix n .~ b, currentPrimed, currentRUp)
          _                 -> s' -- For Forwards, Backwards, Conditionals - they don't change Mill/Store here

        doArithmetic :: EngineState -> EngineState
        doArithmetic (mc', (a, b), arithOp, currentStore, currentPrimed, currentRUp) =
          let newMill = case arithOp of
                Add      -> (a + b, 0)
                Subtract -> (a - b, 0)
                Multiply -> (a * b, 0)
                Divide   -> (a `div` b, a `mod` b)
          in (mc', newMill, arithOp, currentStore, currentPrimed, currentRUp)

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
      bindVariableOperation (n, UnboundConditionalForwards) = ConditionalForwards n
      bindVariableOperation (n, UnboundConditionalBackwards) = ConditionalBackwards n
      bindVariableOperation (n, UnboundSetRunUpLever)    = SetRunUpLever n -- New
      bindVariableOperation (n, UnboundClearRunUpLever)  = ClearRunUpLever n -- New


main :: IO ()
main = do
  -- START OF ORIGINAL MAIN LOGIC (for default program)
  let programDirectory = "programs/default/"
  arithmeticString <- readCard $ programDirectory ++ "operations.pc"
  numericFile <- readCard $ programDirectory ++ "numbers.pc"
  distributiveFile <- readCard $ programDirectory ++ "loadStore.pc"

  let rawOperationCards = filter (not . T.null) $ T.splitOn "-\r\n" arithmeticString
  let operations :: [UnboundOperation]
      operations = parseOperation . lines . T.unpack <$> rawOperationCards

  let rawDistributiveCards = filter (not . T.null) $ T.splitOn "-\r\n" distributiveFile
  let distributive :: [Int]
      distributive = parseVariable . lines . T.unpack <$> rawDistributiveCards

  let rawNumericCards = filter (not . T.null) $ T.splitOn "-\r\n" numericFile
  let numbers :: [Integer]
      numbers = parseNumeric . lines . T.unpack <$> rawNumericCards

  -- Example operations from original main, for context
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

  putStrLn "--- Default Program Execution (from original main logic) ---"
  let boundDefaultOps = bindOperations (distributive ++ inputVars) (operations ++ inputOps) -- Combine default and example
  let defaultParams = numbers ++ inputParams

  -- Check if boundDefaultOps or defaultParams are empty to avoid runtime errors if files are empty
  if null boundDefaultOps || null defaultParams
  then putStrLn "Warning: Default program operations or parameters are empty. Skipping default execution part."
  else do
    let computedDefaultValues = applyParameters defaultParams boundDefaultOps
    putStrLn $ "Default Program Initial Ops (first 20): " ++ show (prettyPrintEither . take 20 $ boundDefaultOps)
    putStrLn $ "Default Program Initial Params (first 20): " ++ show (take 20 defaultParams)
    putStrLn $ "Default Program Computed Values (first 10 states, first 10 store values per state):\n" ++ unlines (map (show . take 10) . take 10 $ computedDefaultValues)
    -- Safer way to print filtered values, checks length
    let filteredValues = dropWhile (\s -> length s > 5 && s !! 5 == 0) . map (take 10) $ computedDefaultValues
    if null filteredValues
    then putStrLn "Default Program Filtered Values: (No states left after filtering or all states have less than 6 elements)"
    else putStrLn $ "Default Program Filtered Values (first 10 store values after filtering):\n" ++ unlines (map show . take 10 $ filteredValues)


  -- New Test Cases
  putStrLn "\n\n--- Conditional Movement Tests ---"

  -- Test Case 1
  putStrLn "\n--- Test Case 1: Conditional Forward - Lever Active (P1 in Store Idx 2) ---"
  let testOps1 = [
          Right UnboundSetRunUpLever,       -- Idx0, Var 0 (dummy)
          Right UnboundSupplyZeroing,       -- Idx1, Var 0 (P1=10 from params)
          Right UnboundConditionalForwards, -- Idx2, Var 1 (Jump 1 op)
          Right UnboundSupplyZeroing,       -- Idx3, Var 1 (P2=20 from params) -- SKIPPED
          Right UnboundStore,               -- Idx4, Var 2 (Store Mill A to result loc) -- EXECUTED AFTER JUMP
          Right UnboundClearRunUpLever      -- Idx5, Var 0 (dummy)
          ]
  let testVars1 = [0, 0, 1, 1, 2, 0]
  let testParams1 = [10, 20, 0] -- P1, P2, ResultLoc
  let boundOps1 = bindOperations testVars1 testOps1
  putStrLn $ "Ops1: " ++ show (prettyPrintEither boundOps1)
  let results1 = applyParameters testParams1 boundOps1
  putStrLn $ "Results1 (Store after each op, expecting P1=10 in Idx2 -> e.g., final state like [0,20,10...]):\n" ++ unlines (map (show . take 5) results1)

  -- Test Case 2
  putStrLn "\n--- Test Case 2: Conditional Forward - Lever Inactive (P1+P2 in Store Idx 2) ---"
  let testOps2 = [
          Right UnboundClearRunUpLever,     -- Var 0 (dummy)
          Right UnboundSupplyZeroing,       -- Var 0 (P1=5)
          Right UnboundConditionalForwards, -- Var 1 (Jump 1 op, but lever off)
          Right UnboundSupplyZeroing,       -- Var 1 (P2=5) -- NOT SKIPPED
          Left Add,
          Right UnboundStore                -- Var 2 (Store Mill A to idx 2)
          ]
  let testVars2 = [0, 0, 1, 1, 2]
  let testParams2 = [5, 5, 0]   -- P1, P2, ResultLoc
  let boundOps2 = bindOperations testVars2 testOps2
  putStrLn $ "Ops2: " ++ show (prettyPrintEither boundOps2)
  let results2 = applyParameters testParams2 boundOps2
  putStrLn $ "Results2 (Store after each op, expecting P1+P2=10 in Idx2 -> e.g., final state like [0,0,10...]):\n" ++ unlines (map (show . take 5) results2)

  -- Test Case 3
  putStrLn "\n--- Test Case 3: Conditional Backward - Lever Active (P1+P2 in Store Idx 2 after jump) ---"
  let testOps3 = [
          Right UnboundSetRunUpLever,       -- idx0. Lever ON. Var 0 (dummy)
          Right UnboundSupplyZeroing,       -- idx1. OpA: Supply P1 (P1_val=7 from params idx0). Var 0
          Right UnboundSupplyZeroing,       -- idx2. OpB: Supply P2 (P2_val=8 from params idx1). Var 1
          Right UnboundConditionalBackwards,-- idx3. CondBack to OpA (idx1). Jump k=2 steps. Var for op should be k+1 = 3.
          Right UnboundSupplyZeroing,       -- idx4. OpC: Supply P1 (P1_val=7 from params idx0) again. Var 0
          Left Add,                         -- idx5.
          Right UnboundStore                -- idx6. Store to Res (params idx2). Var 2
          ]
  let testVars3 = [0, 0, 1, 3, 0, 2]
  let testParams3 = [7, 8, 0]     -- P1_val, P2_val, ResultLoc
  let boundOps3 = bindOperations testVars3 testOps3
  putStrLn $ "Ops3: " ++ show (prettyPrintEither boundOps3)
  let results3 = applyParameters testParams3 boundOps3
  putStrLn $ "Results3 (Store after each op, expecting P1+P2=15 in Idx2 -> e.g., final state like [0,0,15...]):\n" ++ unlines (map (show . take 5) results3)

  -- Test Case 4
  putStrLn "\n--- Test Case 4: Conditional Backward - Lever Inactive (P1+P2 in Store Idx 2, no jump) ---"
  let testOps4 = [
          Right UnboundClearRunUpLever,      -- Var 0 (dummy)
          Right UnboundSupplyZeroing,        -- Var 0 (P1=3)
          Right UnboundSupplyZeroing,        -- Var 1 (P2=3)
          Right UnboundConditionalBackwards, -- Var 3 (Jump 2, but lever off)
          Left Add,
          Right UnboundStore                 -- Var 2 (Store to idx 2)
          ]
  let testVars4 = [0,0,1,3,2]
  let testParams4 = [3,3,0]
  let boundOps4 = bindOperations testVars4 testOps4
  putStrLn $ "Ops4: " ++ show (prettyPrintEither boundOps4)
  let results4 = applyParameters testParams4 boundOps4
  putStrLn $ "Results4 (Store after each op, expecting P1+P2=6 in Idx2 -> e.g., final state like [0,0,6...]):\n" ++ unlines (map (show . take 5) results4)

  -- Test Case 5
  putStrLn "\n--- Test Case 5: Unconditional Forwards (P1 in Store Idx 2) ---"
  let testOps5 = [
          Right UnboundSupplyZeroing,       -- Var 0 (P1=99)
          Right UnboundForwards,            -- Var 1 (Jump 1 op)
          Right UnboundSupplyZeroing,       -- Var 1 (P2=0) -- SKIPPED
          Right UnboundStore                -- Var 2 (Store Mill A to idx 2)
          ]
  let testVars5 = [0, 1, 1, 2]
  let testParams5 = [99, 0, 0] -- P1, P2(dummy), Res
  let boundOps5 = bindOperations testVars5 testOps5
  putStrLn $ "Ops5: " ++ show (prettyPrintEither boundOps5)
  let results5 = applyParameters testParams5 boundOps5
  putStrLn $ "Results5 (Store after each op, expecting P1=99 in Idx2 -> e.g., final state like [0,0,99...]):\n" ++ unlines (map (show . take 5) results5)

  return () -- Ensures main has type IO ()
  where -- <<< ADDED THIS LINE BACK

    -- Helper functions (readCard, prettyPrintEither, parseOperation, etc.) remain below this line
    -- and should not be touched by this diff.

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
          | card == conditionalForwardsCard  -> Right UnboundConditionalForwards
          | card == conditionalBackwardsCard -> Right UnboundConditionalBackwards
          | card == setRunUpLeverCard    -> Right UnboundSetRunUpLever -- New
          | card == clearRunUpLeverCard  -> Right UnboundClearRunUpLever -- New
          | True                             -> error ("unknown operation card: " ++ show card)
      where
        addCard                   = ["* ", "  ", "  ", "  ", "  "]
        subtractCard              = [" *", "  ", "  ", "  ", "  "]
        multiplyCard              = ["  ", "* ", "  ", "  ", "  "]
        divideCard                = ["  ", " *", "  ", "  ", "  "]
        loadPreserveCard          = ["  ", "  ", "* ", "  ", "  "]
        loadZeroCard              = ["  ", "  ", " *", "  ", "  "]
        storeCard                 = ["  ", "  ", "  ", "* ", "  "]
        storePrimedCard           = ["  ", "  ", "  ", " *", "  "]
        forwardsCard              = ["* ", "  ", "  ", "  ", "* "]
        backwardsCard             = [" *", "  ", "  ", "  ", "* "]
        conditionalForwardsCard   = ["**", "  ", "  ", "  ", "  "]
        conditionalBackwardsCard  = ["  ", "**", "  ", "  ", "  "]
        setRunUpLeverCard         = ["* ", "  ", "**", "  ", "  "] -- New S-shape
        clearRunUpLeverCard       = [" *", "  ", "**", "  ", "  "] -- New C-shape

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
