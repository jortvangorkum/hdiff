{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Lib where

import           CommandLine                (Options (..))

import           Control.Monad.Identity     (Identity (runIdentity))
import           Data.Functor.Const         (Const (..))
import           Data.Functor.Identity
import qualified Data.Map                   as M
import           Data.Maybe                 (fromJust)
import           Data.Word                  (Word64)
import           Generics.Simplistic        (Generic (Rep), SRep, V1 (..),
                                             repLeaves, repLeavesList, repMap,
                                             repMapM, toS)
import           Generics.Simplistic.Deep   (CompoundCnstr,
                                             HolesAnn (Hole', Prim', Roll'),
                                             PrimCnstr, SFixAnn, cataM)
import           Generics.Simplistic.Digest (Digest (getDigest), toW64s)
import qualified Generics.Simplistic.Digest as D
import           Generics.Simplistic.Util   (All, Exists (Exists), exElim)
import           Languages.Interface
import           Languages.Main
import           Options.Applicative        (execParser)
import           Preprocess                 (PrepData (..), PrepFix, decorate,
                                             getHash, getHeight, getW64s)
import           System.Exit
import           WordTrie                   as T

mainAST :: Maybe String -> Options -> IO ExitCode
mainAST ext opts = withParsed1 ext mainParsers (optFileA opts)
  $ \_ fa -> do
    print fa
    return ExitSuccess

data Tree a = Leaf
            | Node (Tree a) a (Tree a)

cataT :: b
      -> (b -> a -> b -> b)
      -> Tree a -> b
cataT f _ Leaf         = f
cataT f g (Node l x r) = g (cataT f g l) x (cataT f g r)

cataTM :: Monad m => m b
       -> (m b -> a -> m b -> m b)
       -> Tree a -> m b
cataTM f _ Leaf         = f
cataTM f g (Node l x r) = g (cataTM f g l) x (cataTM f g r)

testTree :: Tree Int
testTree = Node (Node Leaf 1 Leaf) 2 Leaf

testFoldT :: Tree Int -> [Int]
testFoldT = cataT leaf node
  where
    leaf = [0]
    node l x r = l ++ [x] ++ r

testFoldTM :: Tree Int -> [Int]
testFoldTM = runIdentity . cataTM leaf node
  where
    leaf = Identity [0]
    node :: Monad m => m [Int] -> Int -> m [Int] -> m [Int]
    node l x r = do
      l' <- l
      r' <- r
      return $ l' ++ [x] ++ r'

cataP :: Monoid m =>
      (forall x . Const (PrepData a) x -> V1 x -> m)
      -> (forall x . PrimCnstr kappa fam x => Const (PrepData a) x -> x -> m)
      -> (forall x . CompoundCnstr kappa fam x => Const (PrepData a) x -> m -> m)
      -> PrepFix a kappa fam ix -> m
cataP f g h (Hole' ann x) = f ann x
cataP f g h (Prim' ann x) = g ann x
cataP f g h (Roll' ann x) = h ann x'
  where
    x' = repLeaves (cataP f g h) (<>) mempty x

testFold :: PrepFix a kappa fam ix -> M.Map String Int
testFold = cataP f g h
  where
    f ann x = M.empty
    g ann x = M.insert (take 5 (getHash ann)) 0 M.empty
    h ann x = M.insert (take 5 (getHash ann)) xs x
      where
        xs = if not $ null (M.elems x)
             then 1 + maximum (M.elems x)
             else 0

foldPrepFixToTrie :: PrepFix a kappa fam ix -> Trie Int
foldPrepFixToTrie = cataP hole prim roll
  where
    hole ann x = mempty
    prim ann x = T.insert (getHeight ann) (getW64s ann) T.empty
    roll ann   = T.insert (getHeight ann) (getW64s ann)

mainDiff :: Maybe String -> Options -> IO ExitCode
mainDiff ext opts = withParsed2 ext mainParsers (optFileA opts) (optFileB opts)
  $ \_ fa fb -> do
    let decFa = decorate fa
    print decFa

    let hashMap = testFold decFa
    print hashMap

    let trie = foldPrepFixToTrie decFa
    print trie

    return ExitSuccess

mainBody :: Maybe String -> Options -> IO ExitCode
mainBody ext parser = case parser of
  AST{}  -> mainAST ext parser
  Diff{} -> mainDiff ext parser
