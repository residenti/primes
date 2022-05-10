module Primes where

sieve :: [Int] -> [Int]
sieve [] = []
sieve (nextPrime:rest) = nextPrime : sieve noFactors
  where noFactors = filter (not . (==0) . (`mod` nextPrime)) rest

-- maxBound :: Int -> 9223372036854775807
-- Limit the maximum value to 10000.
primes :: [Int]
primes = sieve [2 .. 10000]
