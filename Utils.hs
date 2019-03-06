module Utils (
    factorial,
    choose
) where

-- Factorial
factorial :: Integer -> Integer
factorial x = product [2..x]

-- Binomial Coefficient
choose :: Integer -> Integer -> Integer
n `choose` k
    | k < 0     = 0
    | k > n     = 0
    | otherwise = factorial n `div` (factorial (n - k) * factorial k)
