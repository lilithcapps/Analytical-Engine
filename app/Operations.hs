module Operations where
import           Cards         (MathsOperation (..), Operation,
                                OutputOperation (..), ProcessOperation (..),
                                VariableOperation (..))
import           Control.Lens  (ix, (&), (.~))
import           Control.Monad ((<=<), (>=>))
import           Minecart      (Minecart, MonadMinecart (..))


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
          (Variable o@(SupplyRetaining _), Second) -> doArithmetic . doDistributive o $ s
          (Variable o@(SupplyZeroing _), Second) -> doArithmetic . doDistributive o $ s
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
          let (e, lev) = case operation s of
                Addition    -> ((a + b, ex'), lever s)
                Subtraction -> ((a - b, ex'), (a - b < 0) || lever s)
                Multiply    -> ((a * b, ex'), lever s)
                Divide      -> ((a `div` b, a `mod` b), lever s)
          in s { egress = e, lever = lev }
