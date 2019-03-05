module Oasis (
    a000045
) where

-- A000045
-- Fibonacci numbers
--
-- F(n) = F(n-1) + F(n-2) with F(0) = 0 and F(1) = 1
--
a000045 :: [Integer]
a000045 = fibs
fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
