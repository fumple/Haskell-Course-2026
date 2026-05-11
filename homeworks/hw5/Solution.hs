import Control.Monad.State (State, MonadTrans (lift), modify, MonadState (get, put), runState, evalState, execState, gets)
import Data.Map (Map, empty, (!), insert, lookup)

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