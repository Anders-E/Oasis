module Oasis (
    a000045,
    a000108,
    a000110
) where

import Utils

-- A000004
-- The zero sequence:
--
-- a(n) = 0 for all integer n.
--
a000004 :: [Integer]
a000004 = repeat 0

-- A000005
-- d(n) (also called tau(n) or sigma_0(n)):
--
-- The number of divisors of n.
--
a000005 :: [Integer]
a000005 = map numOfDivs [1..]
numOfDivs :: Integer -> Integer
numOfDivs n = sum [1 | x <- [1..nSquared + 1], n `mod` x == 0]
    where nSquared = floor (fromIntegral n ** 2)

-- A000012
-- The all 1's sequence:
--
-- f(n) = 1
--
a000011 :: [Integer]
a000011 = repeat 1

-- A000045
-- Fibonacci numbers:
--
-- F(n) = F(n-1) + F(n-2) with F(0) = 0 and F(1) = 1.
--
a000045 :: [Integer]
a000045 = fibs
fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

-- A000108
-- Catalan numbers:
-- 
-- C(n) = binomial(2n,n)/(n+1) = (2n)!/(n!(n+1)!). Also called Segner numbers. 
--
a000108 :: [Integer]
a000108 = map catalan [0..]
catalan :: Integer -> Integer
catalan n = (2 * n) `choose` n `div` (n + 1)

-- A000110
-- Bell or exponential numbers:
--
-- Number of ways to partition a set of n labeled elements.
--
a000110 :: [Integer]
a000110 = map head bellTriangle

bellTriangle :: [[Integer]]
bellTriangle' :: Integer -> [Integer]
bellTriangle = map bellTriangle' [0..]
bellTriangle' 0 = [1]
bellTriangle' n = [sum (take n prev') | n <- [1..len]]
    where
        prev = bellTriangle' (n - 1)
        prev' = last prev : prev
        len = length prev'
