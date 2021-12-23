module Main where

import           Criterion.Main
import qualified Data.Map       as M
import           GenericTree

generateTreeG :: Int -> TreeG Int
generateTreeG = from . generateTreeF
  where
    generateTreeF 0 = In $ LeafF 0
    generateTreeF n = In $ NodeF (generateTreeF (n - 1)) n (generateTreeF (n - 1))

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
benchCataMerkleWithMap n = env (setupMapMerkleTree n) (bench (show n) . nf (\(mt, m) -> cataMerkleWithMap m mt))

benchChangeCataMerkleWithMap :: Int -> Benchmark
benchChangeCataMerkleWithMap n = env (setupMapMerkleTreeChange n) (bench (show n) . nf (\(mt, m) -> cataMerkleWithMap m mt))

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
  [ bgroup "Generate Tree"
      [ benchTree 1
      , benchTree 5
      , benchTree 10
      , benchTree 15
      , benchTree 20
      ]
  , bgroup "Merkelize Tree"
      [ benchMerkleTree 1
      , benchMerkleTree 5
      , benchMerkleTree 10
      , benchMerkleTree 15
      , benchMerkleTree 20
      ]
  , bgroup "Merkelize Tree With Merkle Tree"
      [ benchMerkleTreeWithMerkleTree 1
      , benchMerkleTreeWithMerkleTree 5
      , benchMerkleTreeWithMerkleTree 10
      , benchMerkleTreeWithMerkleTree 15
      , benchMerkleTreeWithMerkleTree 20
      ]
  , bgroup "Merkelize Tree With Single Change Merkle Tree"
      [ benchMerkleTreeWithSingleChange 1
      , benchMerkleTreeWithSingleChange 5
      , benchMerkleTreeWithSingleChange 10
      , benchMerkleTreeWithSingleChange 15
      , benchMerkleTreeWithSingleChange 20
      ]
  , bgroup "Generate Result"
      [ benchResult 1
      , benchResult 5
      , benchResult 10
      , benchResult 15
      , benchResult 20
      ]
  , bgroup "Generate (Result, Map)"
      [ benchCataMerkleMap 1
      , benchCataMerkleMap 5
      , benchCataMerkleMap 10
      , benchCataMerkleMap 15
      , benchCataMerkleMap 20
      ]
  , bgroup "Generate (Result, Map) with Map"
      [ benchCataMerkleWithMap 1
      , benchCataMerkleWithMap 5
      , benchCataMerkleWithMap 10
      , benchCataMerkleWithMap 15
      , benchCataMerkleWithMap 20
      ]
  , bgroup "Generate (Result, Map) with Map Single Change"
      [ benchCataMerkleWithMap 1
      , benchCataMerkleWithMap 5
      , benchCataMerkleWithMap 10
      , benchCataMerkleWithMap 15
      , benchCataMerkleWithMap 20
      ]
  ]

