import Data.Foldable (Foldable(toList))
exseq = Append (Append (Single 1) (Single 2)) (Single 3)

data Sequence a = Empty | Single a | Append (Sequence a) (Sequence a) deriving Show

-- 1. Functor for Sequence
instance Functor Sequence where
    fmap :: (a -> b) -> Sequence a -> Sequence b
    fmap _ Empty = Empty
    fmap f (Single a) = Single (f a)
    fmap f (Append a b) = Append (fmap f a) (fmap f b)

-- 2. Foldable for Sequence
instance Foldable Sequence where
    foldMap :: Monoid m => (a -> m) -> Sequence a -> m
    foldMap _ Empty = mempty
    foldMap f (Single a) = f a
    foldMap f (Append a b) = foldMap f a <> foldMap f b

seqToList :: Sequence a -> [a]
seqToList = toList

seqLength :: Sequence a -> Int
seqLength = length

-- 3. Semigroup and Monoid for Sequence
instance Semigroup (Sequence a) where
    (<>) :: Sequence a -> Sequence a -> Sequence a
    (<>) a Empty = a
    (<>) Empty a = a
    (<>) a b = Append a b

instance Monoid (Sequence a) where
    mempty :: Sequence a
    mempty = Empty

-- 4. Tail Recursion and Sequence Search
tailElem :: Eq a => a -> Sequence a -> Bool
tailElem a seq = go [seq]
    where
        go ((Append left right):xs) = go (left : right : xs)

        go (Single b:xs)
            | a == b    = True
            | otherwise = go xs

        go (Empty:xs) = go xs
        
        go [] = False

-- 5. Tail Recursion and Sequence Flatten
tailToList :: Sequence a -> [a]
tailToList seq = go [seq] []
    where
        go :: [Sequence a] -> [a] -> [a]

        go ((Append left right):xs) acc = go (left : right : xs) acc

        go (Single a:xs) acc = go xs (acc ++ [a])

        go (Empty:xs) acc = go xs acc
        
        go [] acc = acc

-- 6. Tail Recursion and Reverse Polish Notation
data Token = TNum Int | TAdd | TSub | TMul | TDiv

tailRPN :: [Token] -> Maybe Int
tailRPN seq = go seq []
    where
        go :: [Token] -> [Token] -> Maybe Int

        go ((TNum a):xs) stack = go xs (TNum a : stack)
        go (TAdd:xs) (TNum b:(TNum a:stack)) = go xs (TNum (a+b) : stack)
        go (TSub:xs) (TNum b:(TNum a:stack)) = go xs (TNum (a-b) : stack)
        go (TMul:xs) (TNum b:(TNum a:stack)) = go xs (TNum (a*b) : stack)
        go (TDiv:xs) (TNum b:(TNum a:stack))
            | b /= 0 = go xs (TNum (a `div` b) : stack)
            | otherwise = Nothing
        
        go [] [TNum a] = Just a

        go [] _ = Nothing
        go (x:xs) (y:ys) = Nothing
        go (x:xs) [] = Nothing

-- 7. Expressing functions via foldr and foldl
myReverse :: [a] -> [a] -- use foldl
myReverse = foldl (\acc x -> x:acc) []

myTakeWhile :: (a -> Bool) -> [a] -> [a] -- use foldr
myTakeWhile cond = foldr f []
  where
    f x acc
        | cond x = x : acc
        | otherwise = []

decimal :: [Int] -> Int
decimal = foldl (\acc x -> acc * 10 + x) 0

-- 8. Run-length encoding via folds
encode :: Eq a => [a] -> [(a, Int)]
encode = foldr f []
    where
        f current ((char, count):ys)
            | current == char = (char, count + 1):ys
            | otherwise = (current, 1) : (char, count) : ys
        f current [] = [(current, 1)]

decode :: [(a, Int)] -> [a] -- using foldr (and replicate)
decode = foldr f []
    where
        f (char, count) stack = replicate count char ++ stack