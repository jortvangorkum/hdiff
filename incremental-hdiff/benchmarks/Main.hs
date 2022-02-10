{-# LANGUAGE LambdaCase #-}
module Main where

import           Criterion.Main
import qualified Data.Map       as M
import           GenericTree

-- Generates Tree of size 2n + 1
-- generateTreeG :: Int -> TreeG Int
-- generateTreeG = from . generateTreeF
--   where
--     generateTreeF n = generateBinTree 0 (n - 1)
--     generateBinTree :: Int -> Int -> TreeF Int
--     generateBinTree l u =
--       if u < l
--       then In $ LeafF l
--       else let i = (l + u) `div` 2
--            in In $ NodeF (generateBinTree l (i - 1)) i (generateBinTree (i + 1) u)

sizeTree :: TreeG Int -> Int
sizeTree = cata (\case
  Inl _                           -> 1
  Inr (Pair (Pair (I l, _), I r)) -> 1 + l + r)

generateTreeG :: Int -> TreeG Int
generateTreeG = from . generateTreeF
  where
    generateTreeF 0 = In $ LeafF 5
    generateTreeF n = In $ NodeF (generateTreeF (n - 1)) 5 (generateTreeF (n - 1))

benchTree :: Int -> Benchmark
benchTree n = bench (show n) $ nf generateTreeG n

benchMerkleTree :: Int -> Benchmark
benchMerkleTree n = env (setupTree n) (bench (show n) . nf merkle)

benchMerkleTreeWithMerkleTree :: Int -> Benchmark
benchMerkleTreeWithMerkleTree n = env (setupTreeMerkleTree n) (bench (show n) . nf (\(t, mt) -> genMTWithMT mt t))

benchMerkleTreeWithSingleChange :: Int -> Benchmark
benchMerkleTreeWithSingleChange n = env (setupTreeMerkleTree n) (bench (show n) . nf (\(t, mt) -> genMTWithMT mt (changeSingleLeaf t)))

benchResult :: Int -> Benchmark
benchResult n = env (setupTree n) (bench (show n) . nf cataMerkleInt)

benchCataMerkleMap :: Int -> Benchmark
benchCataMerkleMap n = env (setupMerkleTree n) (bench (show n) . nf cataMerkleMap)

benchCataMerkleWithMap :: Int -> Benchmark
benchCataMerkleWithMap n = env (setupMapMerkleTree n) (bench (show n) . nf (\(mt, m) -> cataMerkleWithMapStop m mt))

benchChangeCataMerkleWithMap :: Int -> Benchmark
benchChangeCataMerkleWithMap n = env (setupMapMerkleTreeChange n) (bench (show n) . nf (\(mt, m) -> cataMerkleWithMapStop m mt))

benchCataMerkleMapFib :: Int -> Benchmark
benchCataMerkleMapFib n = env (setupMerkleTree n) (bench (show n) . nf cataMerkleMapFib)

benchCataMerkleMapFibWithMap :: Int -> Benchmark
benchCataMerkleMapFibWithMap n = env (setupFibMapMerkleTree n) (bench (show n) . nf (\(mt, m) -> cataMerkleMapFibWithMap m mt))

benchCataMerkleMapFibWithMapSingleChange :: Int -> Benchmark
benchCataMerkleMapFibWithMapSingleChange n = env (setupFibMapMerkleTreeSingleChange n) (bench (show n) . nf (\(mt, m) -> cataMerkleMapFibWithMap m mt))

setupTree :: Int -> IO (TreeG Int)
setupTree = return . generateTreeG

setupMerkleTree :: Int -> IO (MerkleTree Int)
setupMerkleTree = return . merkle . generateTreeG

setupTreeMerkleTree :: Int -> IO (TreeG Int, MerkleTree Int)
setupTreeMerkleTree n = do
                        t <- setupTree n
                        mt <- setupMerkleTree n
                        return (t, mt)

setupMapMerkleTree :: Int -> IO (MerkleTree Int, M.Map String Int)
setupMapMerkleTree n = do
                     let m = snd $ cataMap $ generateTreeG n
                     mt <- setupMerkleTree n
                     return (mt, m)

setupFibMapMerkleTree :: Int -> IO (MerkleTree Int, M.Map String Int)
setupFibMapMerkleTree n = do
                          mt <- setupMerkleTree n
                          let m = snd $ cataMerkleMapFib mt
                          return (mt, m)

setupFibMapMerkleTreeSingleChange :: Int -> IO (MerkleTree Int, M.Map String Int)
setupFibMapMerkleTreeSingleChange n = do
                                      let mt = merkle $ changeSingleLeaf $ generateTreeG n
                                      let m = snd $ cataMerkleMapFib mt
                                      return (mt, m)

changeSingleLeaf :: TreeG Int -> TreeG Int
changeSingleLeaf (In (Inl (K _))) = In (Inl (K 10))
changeSingleLeaf (In (Inr (Pair (Pair (I l, x), r)))) = In $ Inr $ Pair (Pair (I (changeSingleLeaf l), x), r)

setupMapMerkleTreeChange :: Int -> IO (MerkleTree Int, M.Map String Int)
setupMapMerkleTreeChange n = do
                             let m = snd $ cataMap $ generateTreeG n
                             let mt = merkle $ changeSingleLeaf $ generateTreeG n
                             return (mt, m)

main :: IO ()
main = defaultMain
  [ bgroup "Generate Tree" $
    map benchTree [1, 10, 100, 1000, 10000]
  , bgroup "Merkelize Tree" $
    map benchMerkleTree [1, 10, 100, 1000, 10000]
  , bgroup "Merkelize Tree With Merkle Tree" $
    map benchMerkleTreeWithMerkleTree [1, 10, 100, 1000, 10000]
  , bgroup "Merkelize Tree With Single Change Merkle Tree" $
    map benchMerkleTreeWithSingleChange [1, 10, 100, 1000, 10000]
  , bgroup "Generate Result" $
    map benchResult [1, 10, 100, 1000, 10000, 100000]
  , bgroup "Generate (Result, Map)" $
    map benchCataMerkleMap [1, 10, 100, 1000, 10000, 100000]
  , bgroup "Generate (Result, Map) with Map" $
    map benchCataMerkleWithMap [1, 10, 100, 1000, 10000, 100000]
  , bgroup "Generate (Result, Map) with Map Single Change" $
    map benchChangeCataMerkleWithMap [1, 10, 100, 1000, 10000]
  , bgroup "Generate (Fib, Map)" $
    map benchCataMerkleMapFib [1, 10, 20, 30, 40]
  , bgroup "Generate (Fib, Map) with Map" $
    map benchCataMerkleMapFibWithMap [1, 10, 20, 30, 40]
  , bgroup "Generate (Fib, Map) with Map Single Change" $
    map benchCataMerkleMapFibWithMapSingleChange [1, 10, 20, 30, 40]
  ]

