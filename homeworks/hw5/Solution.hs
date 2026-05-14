import Control.Monad.State (State, MonadTrans (lift), modify, MonadState (get, put), runState, evalState, execState, gets, StateT (runStateT))
import Data.Map (Map, empty, (!), insert, lookup, keys, fromList)
import Data.Char (isDigit)
import Control.Monad (forM_)

-- # State Monad
-- 1. Stack machine

data Instr = PUSH Int | POP | DUP | SWAP | ADD | MUL | NEG

execInstr :: Instr -> State [Int] ()
execInstr instr = modify $ go instr
    where
        go :: Instr -> [Int] -> [Int]
        go (PUSH val) state = val : state
        go POP (_:xs) = xs
        go POP [] = []
        go DUP (x:xs) = x : x : xs
        go DUP [] = []
        go SWAP (a:b:xs) = b : a : xs
        go SWAP state = state
        go ADD (a:b:xs) = a+b : xs
        go ADD state = state
        go MUL (a:b:xs) = a*b : xs
        go MUL state = state
        go NEG (x:xs) = -x : xs
        go NEG [] = []

execProg :: [Instr] -> State [Int] ()
execProg (x:xs) = do
    execInstr x
    execProg xs
execProg [] = pure ()

runProg :: [Instr] -> [Int]
runProg instructions = execState (execProg instructions) []

-- 2. Expression evaluator with variable bindings
data Expr
  = Num Int
  | Var String
  | Add Expr Expr
  | Mul Expr Expr
  | Neg Expr
  | Assign String Expr   -- bind the value of the expression to the name, return that value
  | Seq  Expr Expr       -- evaluate the left, then the right; return the value of the right

eval :: Expr -> State (Map String Int) Int
eval (Num v) = pure v
eval (Var str) = do
    state :: Map String Int <- get
    pure $ state ! str
eval (Add a b) = do
    va <- eval a
    vb <- eval b
    pure $ va + vb
eval (Mul a b) = do
    va <- eval a
    vb <- eval b
    pure $ va * vb
eval (Neg a) = do
    va <- eval a
    pure (-va)
eval (Assign key expr) = do
    v <- eval expr
    modify (insert key v)
    pure v
eval (Seq left right) = do
    eval left
    eval right

runEval :: Expr -> Int
runEval expr = evalState (eval expr) empty

-- 3. Memoised edit (Levenshtein) distance
editDistM :: String -> String -> Int -> Int -> State (Map (Int, Int) Int) Int
editDistM xs ys i j = do
    cached <- gets $ Data.Map.lookup (i,j)
    let lastCharIdentical = xs!!(i-1) == ys!!(j-1)

    case (cached, (i,j), lastCharIdentical) of
        (Just v, _, _) -> pure v
        (Nothing, (0, _), _) -> do
            modify $ insert (i,j) j
            pure j
        (Nothing, (_, 0), _) -> do
            modify $ insert (i,j) i
            pure i
        (Nothing, (_, _), True) -> do
            v <- d (i-1) (j-1)
            modify $ insert (i,j) v
            pure v
        (Nothing, (_, _), False) -> do
            val1 <- d (i-1) j
            val2 <- d i (j-1)
            val3 <- d (i-1) (j-1)
            let v = 1 + minimum [val1, val2, val3]
            modify $ insert (i,j) v
            pure v
    where
        d = editDistM xs ys

editDistance :: String -> String -> Int
editDistance xs ys = evalState
    (editDistM xs ys (length xs) (length ys))
    empty

-- # StateT and "Treasure Hunters" Game Simulation
data BoardSpace =
      Normal BoardSpace
    | DecisionPoint (Map String BoardSpace) -- pick next space
    | Obstacle BoardSpace -- stop player at this point, last arg is next space
    | Treasure Int BoardSpace -- add x points, last arg is next space
    | Trap Int BoardSpace -- take away x points, last arg is next space
    | Goal
    deriving (Show, Eq, Ord)

data GameState = GameState { boardSpace :: BoardSpace, points :: Int, energy :: Int } deriving (Show, Eq, Ord)
type AdventureGame a = StateT GameState IO a

movePlayer   :: Int -> AdventureGame Int
makeDecision :: [String] -> AdventureGame String

handleLocation :: AdventureGame Bool
playTurn       :: AdventureGame Bool
playGame       :: AdventureGame ()

getDiceRoll      :: IO Int
displayGameState :: GameState -> IO ()
getPlayerChoice  :: [String] -> IO String

esc :: String
esc = "\x1B"
white :: String
white = esc ++ "[" ++ "37" ++ "m"
bwhite :: String
bwhite = esc ++ "[" ++ "97" ++ "m"
gray :: String
gray = esc ++ "[" ++ "90" ++ "m"
red :: String
red = esc ++ "[" ++ "91" ++ "m"
green :: String
green = esc ++ "[" ++ "92" ++ "m"

-- 4. Player movement and decisions
-- movePlayer   :: Int -> AdventureGame Int
movePlayer spaces
    | spaces > 0 = do
        state <- get
        let ps = points state
        let es = energy state
        if es <= 0
            then do
                put $ GameState (boardSpace state) ps 0
                pure 0
            else do
                end <- handleLocation
                if end
                    then pure 0
                    else do
                        moved <- movePlayer $ spaces - 1
                        pure $ moved + 1
    | otherwise = pure 0

-- makeDecision :: [String] -> AdventureGame String
makeDecision options = do
    lift $ getPlayerChoice options

-- 5.
-- handleLocation :: AdventureGame Bool
handleLocation = do
    state <- get
    let ps = points state
    let es = energy state
    case boardSpace state of
        Normal next -> do
            put $ GameState next ps (es - 1)
            pure False
        DecisionPoint decisions -> do
            choice <- lift $ getPlayerChoice $ keys decisions
            put $ GameState (decisions ! choice) ps (es - 1)
            pure False
        Obstacle next -> do
            put $ GameState next ps (es - 5)
            lift $ putStrLn $ "You encountered an obstacle! You lost " ++ red ++ "5" ++ white ++ " energy."
            pure False
        Treasure p next -> do
            put $ GameState next (ps+p) (es - 1)
            lift $ putStrLn $ "You found treasure and got " ++ green ++ show p ++ white ++ " points!"
            pure False
        Trap p next -> do
            put $ GameState next (ps-p) (es - 1)
            lift $ putStrLn $ "You encountered a trap and lost " ++ red ++ show p ++ white ++ " points..."
            pure False
        Goal -> do
            pure True

-- playTurn       :: AdventureGame Bool
playTurn = do
    roll <- lift getDiceRoll
    moved <- movePlayer roll
    lift $ putStrLn $ "Moved " ++ bwhite ++ show moved ++ white ++ " spaces."
    state <- get
    lift $ displayGameState state
    if energy state <= 0
        then pure True
        else do
            case boardSpace state of
                Goal -> pure True
                _ -> pure False

-- playGame       :: AdventureGame ()
playGame = do
    gameOver <- playTurn
    if gameOver
        then do
            lift $ putStrLn "Game over!"
            state <- get
            case boardSpace state of
                Goal -> lift $ putStrLn "You reached the goal!"
                _ -> lift $ putStrLn "You ran out of energy..."
            lift $ displayGameState state
        else do
            lift $ putStrLn "Next turn:"
            playGame

-- 6.
-- getDiceRoll      :: IO Int
getDiceRoll = do
    putStrLn "Please roll the dice."
    putStrLn "What did you roll?"
    go
    where
        go :: IO Int
        go = do
            putStr $ gray ++ "> " ++ white
            line <- getLine
            if not (null line) && all isDigit line
                then pure (read line)
                else do
                    putStrLn "Please provide a number."
                    go

-- displayGameState :: GameState -> IO ()
displayGameState state = do
    let ps = points state
    let es = energy state
    putStr "You currently have "
    putStr $ show ps
    putStr " points and "
    putStr $ show es
    putStrLn " energy."

-- getPlayerChoice  :: [String] -> IO String
getPlayerChoice options = do
    putStrLn "You need to make a choice! Which way to go?"
    let indexedOptions = zip [1..] options
    forM_ indexedOptions $ \(idx, opt) -> do
        putStr $ show idx
        putStr ") "
        putStrLn opt
    line <- getLine
    if not (null line) && all isDigit line
        then do
            let idx = read line :: Int
            if idx <= length options && idx >= 1
                then pure $ options !! (idx - 1)
                else do
                    putStr "Please pick a number between 1 and "
                    putStr $ show $ length options
                    putStrLn "!"
                    getPlayerChoice options
        else do
            putStr "Please pick a number between 1 and "
            putStr $ show $ length options
            putStrLn "!"
            getPlayerChoice options


exampleBoard :: GameState
exampleBoard = GameState (Normal $ Treasure 5 $ DecisionPoint $ fromList [
    ("Unstable bridge", Trap 5 $ Normal Goal),
    ("Side path", Normal $ Normal $ Normal Goal)]) 0 50
exampleBoard2 :: GameState
exampleBoard2 = GameState (Normal $ Treasure 5 $ Trap 5 $ Normal Goal) 0 1