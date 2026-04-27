import Data.Map hiding (map, foldr)
import Control.Monad.Writer

-- 1. Maze navigation
type Pos = (Int, Int)
data Dir = N | S | E | W deriving (Eq, Ord, Show)
type Maze = Map Pos (Map Dir Pos)

-- a)
move :: Maze -> Pos -> Dir -> Maybe Pos
move maze pos dir = do
    curr <- Data.Map.lookup pos maze
    Data.Map.lookup dir curr

-- b)
followPath :: Maze -> Pos -> [Dir] -> Maybe Pos
followPath maze pos (x:xs) = do
    curr <- Data.Map.lookup pos maze
    next <- Data.Map.lookup x curr
    followPath maze next xs
followPath maze pos [] = Just pos

-- c)
safePath :: Maze -> Pos -> [Dir] -> Maybe [Pos]
safePath maze pos (x:xs) = do
    curr <- Data.Map.lookup pos maze
    next <- Data.Map.lookup x curr
    path <- safePath maze next xs
    pure (pos:path)
safePath maze pos [] = Just [pos]

-- 2. Decoding a message
type Key = Map Char Char

decrypt :: Key -> String -> Maybe String
decrypt key = traverse (`Data.Map.lookup` key)

decryptWords :: Key -> [String] -> Maybe [String]
decryptWords key = traverse (decrypt key)

-- 3. Seating arrangements
type Guest = String
type Conflict = (Guest, Guest)
-- TODO: seatings :: [Guest] -> [Conflict] -> [[Guest]]

-- 4. Result monad with warnings
data Result a = Failure String | Success a [String] deriving (Show)

-- a)
instance Functor Result where
    fmap _ (Failure str) = Failure str
    fmap f (Success val warns) = Success (f val) warns

instance Applicative Result where
    pure v = Success v []
    (Failure str) <*> _ = Failure str
    Success f warns <*> Failure str = Failure str
    Success f warns1 <*> Success v warns2 = Success (f v) (warns1 ++ warns2)

instance Monad Result where
    Failure str >>= _ = Failure str
    (Success x warns) >>= f = f x

-- b)
warn    :: String -> Result ()
failure :: String -> Result a

warn str = Success () [str]
failure = Failure

-- c)
validateAge :: Int -> Result Int
validateAge age
    | age < 0 = Failure "Age cannot be negative"
    | age > 150 = Success age ["Warning! Age is above 150"]
    | otherwise = Success age []

validateAges :: [Int] -> Result [Int]
validateAges = mapM validateAge

-- 5. Evaluator with simplification log
data Expr = Lit Int | Add Expr Expr | Mul Expr Expr | Neg Expr deriving (Show)
simplify :: Expr -> Writer [String] Expr
simplify (Add a b) = do
    exprA <- simplify a
    exprB <- simplify b
    simplifyNoRecurse (Add exprA exprB)
simplify (Mul a b) = do
    exprA <- simplify a
    exprB <- simplify b
    simplifyNoRecurse (Mul exprA exprB)
simplify (Neg a) = do
    exprA <- simplify a
    simplifyNoRecurse (Neg exprA)
simplify (Lit v) = writer (Lit v,[])

simplifyNoRecurse :: Expr -> Writer [String] Expr
simplifyNoRecurse (Add (Lit 0) e) = writer (e,["Add identity: 0 + e -> e"])
simplifyNoRecurse (Add e (Lit 0)) = writer (e,["Add identity: e + 0 -> e"])
simplifyNoRecurse (Mul (Lit 1) e) = writer (e,["Mul identity: 1 * e -> e"])
simplifyNoRecurse (Mul e (Lit 1)) = writer (e,["Mul identity: e * 1 -> e"])
simplifyNoRecurse (Mul (Lit 0) _) = writer (Lit 0, ["Zero absorption: 0 * _ -> 0"])
simplifyNoRecurse (Mul _ (Lit 0)) = writer (Lit 0, ["Zero absorption: _ * 0 -> 0"])
simplifyNoRecurse (Neg (Neg e)) = writer (e, ["Double negation: -(-e) -> e"])
simplifyNoRecurse (Add (Lit a) (Lit b)) = writer (Lit (a+b), ["Constant folding: a+b"])
simplifyNoRecurse (Mul (Lit a) (Lit b)) = writer (Lit (a*b), ["Constant folding: a*b"])
simplifyNoRecurse e = writer (e,[])

-- 6. ZipList
newtype ZipList a = ZipList { getZipList :: [a] } deriving (Show)
instance Functor ZipList where
  fmap f list = ZipList (map f (getZipList list))

instance Applicative ZipList where
  pure a = ZipList $ repeat a
  (ZipList (x:xs)) <*> (ZipList (y:ys)) = ZipList list
    where
        curr = x y
        rem = ZipList xs <*> ZipList ys
        list = curr : getZipList rem
  (ZipList []) <*> (ZipList []) = ZipList []
  (ZipList _) <*> (ZipList []) = ZipList []

-- b) works! tried in GHCI

-- c)
-- 