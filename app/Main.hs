{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}
{-# LANGUAGE MultiWayIf        #-}

import           Cards                    hiding (addition, backwards,
                                           backwardsCond, divide, forwards,
                                           forwardsCond, loadPreserve, loadZero,
                                           multiply, store, storePrimed,
                                           subtraction)
import           Control.Lens             (ix, (&), (.~))
import           Control.Monad.State.Lazy
import           Data.Data                (Data (toConstr), showConstr)
import qualified Data.Text.Lazy           as T
import qualified Data.Text.Lazy.IO        as IO
import           Minecart                 (Minecart, backwardsN, dismount,
                                           forwardsN, mount, next)
import           Prelude                  hiding (Left, Right)
import           System.IO                (hFlush, stdout)


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

data OutputValue = PrintV EgressAxis | RingBell | None
  deriving (Show, Eq)

applyParameters :: [Integer] -> [Operation] -> [OutputValue]
applyParameters params ops = filter (/= None) states
  where
    str = params ++ [0,0..] :: StoreValue
    iAxis = ((0, 0), 0) :: IngressAxis
    eAxis = (0, 0) :: EgressAxis

    states :: [OutputValue]
    states = output <$> evaluateState (MkState (mount ops) iAxis eAxis Addition str False False None)
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
                        Addition      -> ((a + b, ex'), lever s)
                        Subtraction -> ((a - b, ex'), (fst eAxis < 0) || lever s)
                        Multiply -> ((a * b, ex'), lever s)
                        Divide   -> ((a `div` b, a `mod` b), lever s)
                  in s { egress = eAxis, lever = lev }

runProgram :: IO ()
runProgram = do
  putStr "Please enter the name of the program to analyse [default]: "
  input <- getLine
  let programDirectory = "programs/" ++ if input /= "" then input else "default"
  arithmeticString <- readCard $ programDirectory ++ "/operations.pc"
  numericFile <- readCard $ programDirectory ++ "/numbers.pc"
  distributiveFile <- readCard $ programDirectory ++ "/loadStore.pc"

  let operations :: [UnboundOperation]
      operations = parseOperation <$> textToList arithmeticString
  let distributive :: [Int]
      distributive = parseVariable <$> textToList distributiveFile
  let numbers :: [Integer]
      numbers = parseNumeric <$> textToList numericFile

  let opChain = bindOperations distributive operations
  let computedValues = applyParameters numbers opChain

  putStrLn . (++) "Initial Operations: " $ show . prettyPrintEither $ operations
  putStrLn $ "Initial Variables: " ++ show distributive
  putStrLn $ "Initial Parameters: " ++ show numbers

  print . take 20 $ opChain
  mapM_ processOutput computedValues

  where
    processOutput :: OutputValue -> IO ()
    processOutput (PrintV a) = print a
    processOutput RingBell   = putChar '\a'
                              >> putStr "Operator Attention - Press [Enter] to resume analysis"
                              >> hFlush stdout
                              >> getLine >> return ()
    processOutput None       = return ()

    readCard :: FilePath -> IO T.Text
    readCard = IO.readFile

    prettyPrintEither :: (Functor f, Show a) => f (ProcessOperation a) -> f String
    prettyPrintEither = (processOperation show show show <$>)

    textToList s = lines . T.unpack <$> T.splitOn "-\r\n" s


    

    conName :: Data a => a -> String
    conName = showConstr . toConstr

  -- let inputOps = [
  --       Math Addition,
  --       Variable UnboundSupplyZeroing,
  --       Variable UnboundSupplyRetaining,
  --       Output Print, Output Bell,
  --       Variable UnboundStore,
  --       Variable UnboundSupplyZeroing,
  --       Variable UnboundSupplyZeroing,
  --       Output Print, Output Bell,
  --       Variable UnboundStore
  --       ]
  -- let inputVars = [0, 1, 5, 1, 5, 6]
  -- let inputParams = [5, 10]

main :: IO ()
main = do
  putStrLn "[r]un a program"
  putStrLn "[c]reate a program"
  input <- readLn :: IO String
  if | ((input == "r") || (input == "return"))   -> runProgram
     | ((input == "r") || (input == "return")) -> createProgram
     | True              -> putStrLn "invalid selection" >> main

  return ()
