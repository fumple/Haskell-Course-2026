newtype Reader r a = Reader { runReader :: r -> a }
-- ^ runReader executes a Reader computation by supplying an environment `r`
--   and returning a result of type `a`.

-- 1. Functor, Applicative, and Monad instances

instance Functor (Reader r) where
  -- fmap :: (a -> b) -> Reader r a -> Reader r b
  fmap f reader = Reader (f . runReader reader)

instance Applicative (Reader r) where
  -- pure   :: a -> Reader r a
  pure a = Reader (const a)
  -- liftA2 :: (a -> b -> c) -> Reader r a -> Reader r b -> Reader r c
  liftA2 f ra rb = Reader (\r -> f (runReader ra r) (runReader rb r))

instance Monad (Reader r) where
  -- (>>=) :: Reader r a -> (a -> Reader r b) -> Reader r b
  (>>=) ra frb = Reader (\r -> runReader (frb (runReader ra r)) r)


-- 2. Primitive operations
-- Retrieves the entire environment.
ask   :: Reader r r
ask = Reader id

-- Retrieves a value derived from the environment by applying a projection,
-- e.g. `asks interestRate :: Reader BankConfig Double`.
asks  :: (r -> a) -> Reader r a
asks = Reader

-- Runs a subcomputation in a locally modified environment. The modification
-- is only visible inside the passed Reader — once it returns, the outer
-- environment is restored (conceptually; there is no mutable state, the
-- modified environment simply goes out of scope).
local :: (r -> r) -> Reader r a -> Reader r a
local f reader = Reader (\r -> runReader reader (f r))

-- 3. A practical example -- banking system
data BankConfig = BankConfig
  { interestRate   :: Double  -- annual interest rate (e.g. 0.05 for 5%)
  , transactionFee :: Int     -- flat fee charged per transaction
  , minimumBalance :: Int     -- minimum required balance on an account
  } deriving (Show)

data Account = Account
  { accountId :: String       -- account identifier
  , balance   :: Int          -- current balance
  } deriving (Show)

-- Computes the interest accrued on the account, based on the configured rate.
-- The result should be an Int — round or truncate as you see fit, but be consistent.
calculateInterest   :: Account -> Reader BankConfig Int
calculateInterest acc = do
    rate <- asks interestRate
    let bal = balance acc
    pure $ round (fromIntegral bal * rate)

-- Deducts the transaction fee from the account and returns the updated account.
-- The accountId should remain unchanged.
applyTransactionFee :: Account -> Reader BankConfig Account
applyTransactionFee acc = do
    let accId = accountId acc
    let bal = balance acc
    fee <- asks transactionFee
    pure $ Account accId (bal - fee)

-- Checks whether the account balance meets the configured minimum.
checkMinimumBalance :: Account -> Reader BankConfig Bool
checkMinimumBalance acc = do
    let bal = balance acc
    minBal <- asks minimumBalance
    pure $ bal >= minBal

-- Runs the three operations above on a single account and combines their results.
-- The returned tuple contains:
--   * the account after the transaction fee has been applied,
--   * the interest computed from the ORIGINAL account,
--   * whether the ORIGINAL account meets the minimum balance requirement.
processAccount      :: Account -> Reader BankConfig (Account, Int, Bool)
processAccount acc = do
    newAcc <- applyTransactionFee acc
    interest <- calculateInterest acc
    meetsReq <- checkMinimumBalance acc
    pure (newAcc, interest, meetsReq)