-- # Homework 01
-- ## List Comprehensions

-- 1. Goldbach Pairs
goldbachPairs :: Int -> [(Int, Int)]
goldbachPairs n | n >= 4 = [(p, q) | q <- primesTo (n - 2), p <- primesTo q, p + q == n]

-- 2. Coprime Pairs
coprimePairs :: [Int] -> [(Int, Int)]
coprimePairs [] = []
coprimePairs (x:xs) = [(x,y) | y <- xs, x < y, gcd x y == 1] ++ [(y,x) | y <- xs, y < x, gcd x y == 1] ++ coprimePairs xs

-- 3. Sieve of Eratosthenes
sieve :: [Int] -> [Int]
sieve [] = []
sieve (x:xs) = x : sieve [i | i <- xs, mod i x /= 0]

primesTo :: Int -> [Int]
primesTo n = sieve [2..n]

isPrime :: Int -> Bool
isPrime n | n >= 2 = last (primesTo n) == n
          | n <  2 = False

-- 4. Matrix Multiplication
matMul :: [[Int]] -> [[Int]] -> [[Int]]
matMul [] [] = []
matMul a b = [[calculateEntry a b i j p | j <- [0..n - 1]] | i <- [0..m - 1]]
    where
        m = length a
        p = length (head a)
        n = length (head b)
        calculateEntry :: [[Int]] -> [[Int]] -> Int -> Int -> Int -> Int
        calculateEntry a b i j p = sum [ a !! i !! k * b !! k !! j | k <- [0 .. p-1] ]

-- 5. Permutations
removeAtIndex :: [a] -> Int -> [a]
removeAtIndex [] _ = []
removeAtIndex list index = let (ys,zs) = splitAt index list in ys ++ tail zs

permutations :: Int -> [a] -> [[a]]
permutations _ [] = []
permutations n list | n == 1 = [[e] | e <- list]
                    | n >  1 =
                        let
                            len = length list
                        in
                            [(list !! i) : perms | i <- [0..(len - 1)], perms <- permutations (n-1) (removeAtIndex list i)]

-- ## Lazy/Eager Evaluation, `seq`, and Bang Patterns

-- 6. Hamming Numbers
merge :: Ord a => [a] -> [a] -> [a]
merge [] l = l
merge l [] = l
merge (x:xs) (y:ys) | x <  y = x : merge xs (y:ys)
                    | x >  y = y : merge (x:xs) ys
                    | x == y = x : merge xs ys

hamming :: [Integer]
hamming = 1 : merge (merge a b) c
    where
        a = map (2*) hamming
        b = map (3*) hamming
        c = map (5*) hamming

-- 7. Integer Power with Bang Patterns
power :: Int -> Int -> Int
power x y = go x y 1
    where
        go :: Int -> Int -> Int -> Int
        go x y !a | y == 0 = a
                  | y >= 1 = go x (y - 1) (a * x)

-- 8. Running Maximum
listMaxA :: [Int] -> Int
listMaxB :: [Int] -> Int

listMaxA list = go list minBound
    where
        go :: [Int] -> Int -> Int
        go []     a = a
        go (x:xs) a | x <= a = seq a (go xs a)
                    | x >  a = seq x (go xs x)

listMaxB list = go list minBound
    where
        go :: [Int] -> Int -> Int
        go []      a = a
        go (x:xs) !a | x <= a = go xs a
                     | x >  a = go xs x

-- 9. Infinite Prime Stream
primes :: [Int]
primes = sieve [2..]

isPrime2 :: Int -> Bool
isPrime2 n = go n primes
    where
        go :: Int -> [Int] -> Bool
        go n (x:xs) | x == n = True
                    | x <  n = go n xs
                    | x >  n = False

-- 10. Strict Accumulation and Space Leaks
-- a)
mean :: [Double] -> Double
mean list = go list 0 0
    where
        go :: [Double] -> Double -> Int -> Double
        go [] sum len = sum / fromIntegral len
        go (x:xs) sum len = go xs (sum + x) (len + 1)

-- b) All elements of the pair need the bang pattern
meanStrict :: [Double] -> Double
meanStrict list = go list (0, 0)
    where
        go :: [Double] -> (Double, Int) -> Double
        go [] (!sum, !len) = sum / fromIntegral len
        go (x:xs) (!sum, !len) = go xs (sum + x, len + 1)

-- c)
meanAndVarianceStrict :: [Double] -> (Double, Double)
meanAndVarianceStrict list = go list (0, 0, 0)
    where
        go :: [Double] -> (Double, Double, Int) -> (Double, Double)
        go [] (!sum, !sumSquared, !len) = (mean, (sumSquared / fromIntegral len) - mean ^ 2)
            where
                mean = sum / fromIntegral len
        go (x:xs) (!sum, !sumSquared, !len) = go xs (sum + x, sumSquared + x^2, len + 1)