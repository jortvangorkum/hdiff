{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards   #-}
module Parse.Print where

import           Control.Monad    (void)
import           Foreign.C.String
import           Foreign.Ptr      (Ptr (..))
import           Parse.Helper
import           TreeSitter.Node
import           TreeSitter.Tree


printTree :: Ptr Tree -> IO ()
printTree = mapPtrTree f
  where
    f node children = void (printNode node)

printNode :: Node -> IO ()
printNode n@Node {..} = do
  theType <- peekCString nodeType
  let TSPoint {..} = nodeStartPoint n
      start        = "(" ++ show pointRow ++ "," ++ show pointColumn ++ ")"
  let TSPoint {..} = nodeEndPoint
      end          = "(" ++ show pointRow ++ "," ++ show pointColumn ++ ")"
  print $ theType ++ start ++ "-" ++ end
