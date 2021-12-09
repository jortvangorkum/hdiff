{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards   #-}
module Parse.Print where

import           Control.Monad         (forM_, void)
import           Foreign.C.String
import           Foreign.Marshal.Alloc (malloc, mallocBytes)
import           Foreign.Marshal.Array (mallocArray)
import           Foreign.Marshal.Utils (new)
import           Foreign.Ptr           (Ptr (..), nullPtr, plusPtr)
import           Foreign.Storable      (peek, peekElemOff, poke)
import           Parse.Helper
import           TreeSitter.Haskell
import           TreeSitter.Node
import           TreeSitter.Parser
import           TreeSitter.Tree


printTree2 :: Ptr Tree -> IO ()
printTree2 = foldPtrTree f
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
