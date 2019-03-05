module Oasis (
    a000045
) where

-- A000045
-- Fibonacci numbers
a000045 :: [Integer]
a000045 = fibs
fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
