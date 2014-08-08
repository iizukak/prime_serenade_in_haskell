module Main where

import ONeillPrimes
main = do let x = primes !! 10000000
          putStrLn $ show x

