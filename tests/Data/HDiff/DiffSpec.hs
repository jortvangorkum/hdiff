module Data.HDiff.DiffSpec
  ( spec
  ) where

import           Data.HDiff.Base
import           Data.HDiff.Diff
import qualified Data.Set                 as S
import           Languages.RTree
import           Languages.RTree.Diff

import           Generics.Simplistic.Deep

import           Test.Hspec
import           Test.QuickCheck

import           Control.Monad            (forM_)


diffWellScopedChanges :: DiffMode -> Property
diffWellScopedChanges mode = forAll genSimilarTrees' $ \(t1 , t2)
  -> let patch = hdiffRTreeHM mode 1 t1 t2
      in go $ chgDistr patch
  where
    go :: Chg fam prim ix -> Property
    go (Chg del ins)
      = let vd = S.fromList $ holesHolesList del
            vi = S.fromList $ holesHolesList ins
         in property $ vd == vi

applyCorrectness :: DiffMode -> Property
applyCorrectness mode = forAll genSimilarTrees' $ \(t1 , t2)
  -> let patch = hdiffRTreeHM mode 1 t1 t2
      in case applyRTree patch t1 of
           Left err -> counterexample ("Apply failed with: " ++ err) False
           Right r  -> property $ t2 == r

diffModeSpec :: DiffMode -> Spec
diffModeSpec mode = do
  describe "diff" $ do
    it "produce well-scoped changes" $ do
      diffWellScopedChanges mode
  describe "apply" $ do
    it "is correct" $ do
      applyCorrectness mode

spec :: Spec
spec = do
  forM_ (enumFrom (toEnum 0)) $ \m ->
    describe ("Extraction (" ++ show m ++ ")") $ diffModeSpec m
