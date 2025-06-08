{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}
{-# LANGUAGE MultiWayIf        #-}

import           Cards                    (DistributiveRecord (name),
                                           MathsOperation (..), NumberPunchCard,
                                           NumericPunchCard, Operation,
                                           OperationPunchCard,
                                           OutputOperation (Bell, Print),
                                           ProcessOperation (..), PunchCard,
                                           UnboundOperation,
                                           VariableOperation (..),
                                           bindOperations, isDistributive,
                                           parseNumeric, parseOperation,
                                           parseOperationToString,
                                           parseStringToOperation,
                                           parseStringToParameter,
                                           parseStringToVariable, parseVariable,
                                           processOperation)
import qualified Cards                    as C

import           Control.Lens             (ix, (&), (.~))
import           Control.Monad.State.Lazy
import           Data.Data                (Data (toConstr), showConstr)
import           Data.Maybe               (fromMaybe)
import qualified Data.Text.Lazy           as T
import qualified Data.Text.Lazy.IO        as IO
import           Minecart                 (Minecart, backwardsN, dismount,
                                           forwardsN, mount, next)
import           Prelude                  hiding (Left, Right)
import           System.Directory         (createDirectoryIfMissing)
import           System.FilePath.Windows  (takeDirectory)
import           System.IO                (hFlush, stdout)


type MinecartM = Maybe (Minecart Operation [Operation] [Operation])
type IngressAxis = ((Integer, Integer), Integer)
type EgressAxis = (Integer, Integer)
type StoreValue = [Integer]
data Axis = First | Second
type RunUpLever = Bool

data EngineState = MkState {
  minecart  :: MinecartM,
  ingress   :: IngressAxis,
  egress    :: EgressAxis,
  operation :: MathsOperation,
  store     :: StoreValue,
  axis      :: Axis,
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
    states = output <$> evaluateState (MkState (mount ops) iAxis eAxis Addition str First False None)
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
                Just op -> case (op, axis s) of
                  (Variable op@(SupplyRetaining _), Second) -> doArithmetic . doDistributive op $ s
                  (Variable op@(SupplyZeroing _), Second) -> doArithmetic . doDistributive op $ s
                  (Variable r, _) -> doDistributive r s
                  (Output Print, _) -> s { output = PrintV (egress s) }
                  (Output Bell, _) -> s { output = RingBell }
                  (Math l, _) -> s { operation = l }
              where
                doDistributive :: VariableOperation -> EngineState -> EngineState
                doDistributive op s@(MkState { ingress = (ing@(_, a'), b), egress = (ex, ex'), store = str }) = case op of
                  SupplyRetaining n -> case axis s of
                    First  -> s { axis = Second, ingress = ((str !! n, a'), b) }
                    Second -> s { axis = First, ingress = (ing, str !! n) }
                  SupplyZeroing n   -> case axis s of
                    First -> s { axis = Second, ingress = ((str !! n, a'), b), store = str & ix n .~ 0 }
                    Second -> s { axis = First, ingress = (ing, str !! n), store = str & ix n .~ 0 }
                  Store n           -> s { store = str & ix n .~ ex}
                  StorePrimed n     -> s { store = str & ix n .~ ex'}
                  _                 -> s

                doArithmetic :: EngineState -> EngineState
                doArithmetic s@(MkState { ingress = ((a, a'), b), egress = (_, ex')}) =
                  let (eAxis, lev) = case operation s of
                        Addition    -> ((a + b, ex'), lever s)
                        Subtraction -> ((a - b, ex'), (a - b < 0) || lever s)
                        Multiply    -> ((a * b, ex'), lever s)
                        Divide      -> ((a `div` b, a `mod` b), lever s)
                  in s { egress = eAxis, lever = lev }

runProgram :: IO ()
runProgram = do
  putStr "Please enter the name of the program to analyse [default]: " >> hFlush stdout
  input <- getLine
  let programDirectory = "programs\\" ++ if input /= "" then input else "square"
  arithmeticString <- readCard $ programDirectory ++ "\\operations.pc"
  numericFile <- readCard $ programDirectory ++ "\\parametric.pc"
  distributiveFile <- readCard $ programDirectory ++ "\\distributive.pc"

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

  -- print . take 20 $ opChain
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

    textToList s = lines . T.unpack <$> T.splitOn "-\n" s


createProgram :: IO ()
createProgram = do
  putStr "enter the program name: " >> hFlush stdout
  n <- getLine
  putStrLn "enter the operations"
  putStrLn "press enter after each operation"
  putStrLn "press enter with no input when you have entered all the required operations"
  putStrLn $ "valid operations are: "
      ++ "\n\t mathematical: "
      ++ name C.addition ++ " | "
      ++ name C.subtraction ++ " | "
      ++ name C.multiply ++ " | "
      ++ name C.divide
      ++ "\n\t informational: "
      ++ name C.write ++ " | "
      ++ name C.bell
      ++ "\n\t distributive: "
      ++ name C.loadPreserve ++ " | "
      ++ name C.loadZero ++ " | "
      ++ name C.store ++ " | "
      ++ name C.storePrimed ++ " | "
      ++ name C.forwards ++ " | "
      ++ name C.backwards ++ " | "
      ++ name C.forwardsCond ++ " | "
      ++ name C.backwardsCond

  ops <- getOps
  _ <- writeCards n "operations" ops

  putStrLn "enter the distributive variables"
  putStrLn "press enter after each variable"
  putStrLn "variables should be unsigned, and between 1 and 999 (inclusive)"
  putStrLn "the operation the variable will be used for will be printed before each input"
  vars <- mapM getVar . filter isDistributive $ ops
  _ <- writeCards n "distributive" vars

  putStrLn "enter the parametric variables"
  putStrLn "press enter after each variable"
  putStrLn "variables are signed, and may be up to 50 digits long"
  putStrLn "the positive sign may be omitted"
  putStrLn "press enter with no input when you have entered all the required parameters"
  params <- getParams
  _ <- writeCards n "parametric" params

  return ()

  where
    getVar :: OperationPunchCard -> IO NumberPunchCard
    getVar op = do
      putStr (fromMaybe "" (parseOperationToString op) ++ ": ") >> hFlush stdout
      v <- getLine
      case parseStringToVariable v of
        Nothing  -> putChar '\a' >> putStrLn "invalid input" >> getVar op
        Just var -> return var

    getOps :: IO [OperationPunchCard]
    getOps = do
      o <- getLine
      if o == ""
      then return []
      else do
        case parseStringToOperation o of
          Nothing -> putChar '\a' >> putStrLn "invalid operation" >> getOps
          Just v  -> getOps >>= return . (v:)

    getParams :: IO [NumericPunchCard]
    getParams = do
      p <- getLine
      if p == ""
      then return []
      else do
        case parseStringToParameter p of
          Nothing    -> putChar '\a' >> putStrLn "invalid parameter" >> getParams
          Just param -> getParams >>= return . (param:)

    writeCards :: String -> String -> [PunchCard] -> IO ()
    writeCards name filename op = createAndWriteFile ("programs\\" ++ name ++ "\\" ++ filename ++ ".pc")
      $ T.unpack . T.intercalate "-\n" $ (T.pack . unlines <$> op)
      where
        createAndWriteFile :: FilePath -> String -> IO ()
        createAndWriteFile path content = do
          createDirectoryIfMissing True $ takeDirectory path

          writeFile path content

    conName :: Data a => a -> String
    conName = showConstr . toConstr

  -- putStrLn "[e]dit   - edit a program"


main :: IO ()
main = do
  putStrLn "[r]un    - run a program"
  putStrLn "[c]reate - create a program"
  putStrLn "[q]uit   - exit the program"
  input <- getLine :: IO String
  if | ((input == "r") || (input == "return"))   -> runProgram >> main
     | ((input == "c") || (input == "create")) -> createProgram >> main
     | ((input == "q") || (input == "quit")) -> return ()
     | True              -> putStrLn "invalid selection" >> main

  return ()
