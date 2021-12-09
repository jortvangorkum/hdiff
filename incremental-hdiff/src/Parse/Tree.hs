{-# LANGUAGE RecordWildCards #-}
module Parse.Tree where

import           Foreign.C       (peekCString)
import           Foreign.Ptr     (Ptr (..))
import           Parse.Helper
import           TreeSitter.Node
import           TreeSitter.Tree

data ParseTree = PNode String [ParseTree]
  deriving (Eq, Show)

convertPtrToTree :: Ptr Tree -> IO ParseTree
convertPtrToTree = cataPtrTree f
  where
    f Node {..} children = do
      nodeName <- peekCString nodeType
      return (PNode nodeName children)
