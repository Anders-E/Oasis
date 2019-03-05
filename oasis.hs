module Oasis (
    a000045,
    a000110
) where

-- A000045
-- Fibonacci numbers:
--
-- F(n) = F(n-1) + F(n-2) with F(0) = 0 and F(1) = 1.
--
a000045 :: [Integer]
a000045 = fibs
fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

-- A000110
-- Bell or exponential numbers:
--
-- Number of ways to partition a set of n labeled elements.
--
a000110 :: [Integer]
a000110 = bells

bells :: [Integer]
bells = map head bellTriangle

bellTriangle :: [[Integer]]
bellTriangle' :: Integer -> [Integer]
bellTriangle = map bellTriangle' [0..]
bellTriangle' 0 = [1]
bellTriangle' n = [sum (take n prev') | n <- [1..len]]
    where
        prev = bellTriangle' (n - 1)
        prev' = last prev : prev
        len = length prev'
