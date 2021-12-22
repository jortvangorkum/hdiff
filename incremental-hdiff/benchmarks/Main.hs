module Main where

import           Criterion.Main
import           GenericTree

fib :: (Ord a, Num a, Num p) => a -> p
fib m
  | m < 0 = error "Negative"
  | otherwise = go m
  where
    go 0 = 0
    go 1 = 1
    go n = go (n - 1) + go (n - 2)

generateTreeG :: Int -> TreeG Int
generateTreeG = from . generateTreeF
  where
    generateTreeF 0 = In $ LeafF 0
    generateTreeF n = In $ NodeF (generateTreeF (n - 1)) n (generateTreeF (n - 1))

benchTree :: Int -> Benchmark
benchTree n = bench (show n) $ nf generateTreeG n

main :: IO ()
main = defaultMain [
    bgroup "Generate Tree" [ benchTree 1
                           , benchTree 5
                           , benchTree 10
                           , benchTree 15
                           , benchTree 20
                           ]
  ]

