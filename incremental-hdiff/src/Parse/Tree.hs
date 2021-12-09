{-# LANGUAGE RecordWildCards #-}
module Parse.Tree where

import           Control.Monad    (forM, forM_)
import           Foreign.C        (CString, peekCString)
import           Foreign.Ptr      (Ptr (..), nullPtr, plusPtr)
import           Foreign.Storable (peek, peekElemOff, poke)
import           Parse.Helper
import           TreeSitter.Node
import           TreeSitter.Tree


data ParseTree = PNode String [ParseTree]
  deriving (Eq, Show)

convertPtrToTree :: Ptr Tree -> IO ParseTree
convertPtrToTree = foldPtrTree f
  where
    f Node {..} children = do
      nodeName <- peekCString nodeType
      return (PNode nodeName children)
