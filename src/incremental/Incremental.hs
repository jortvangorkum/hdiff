{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving    #-}

module Incremental where

import           Data.HDiff.Base
import           Data.HDiff.Diff
import           Generics.Simplistic.Deep

import           GHC.Generics             (Generic)

import           Data.HDiff.Diff.Types    (diffOptionsDefault)

data RTree = String :>: [RTree]
  deriving (Eq , Show)

type RTreePrims = '[ String ]
type RTreeFam   = '[ RTree , [RTree] ]
type PatchRTree = Patch RTreePrims RTreeFam RTree

deriving instance Generic RTree
instance Deep RTreePrims RTreeFam RTree
instance Deep RTreePrims RTreeFam [ RTree ]

dfromRTree :: RTree -> SFix RTreePrims RTreeFam RTree
dfromRTree = dfrom

hdiffRTreeHM :: DiffMode -> Int -> RTree -> RTree -> PatchRTree
hdiffRTreeHM m h a b = diffOpts (diffOptionsDefault { doMode = m
                                                    , doMinHeight = h
                                                    })
                                (dfromRTree a)
                                (dfromRTree b)
