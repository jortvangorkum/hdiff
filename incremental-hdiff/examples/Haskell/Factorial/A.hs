module Factorial where

factorial :: Int -> Int
factorial 0 = 1
factorial i = i * factorial (i-1)