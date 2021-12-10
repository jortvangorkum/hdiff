{-# LANGUAGE RecordWildCards #-}
module Parse.Helper where

import           Control.Monad         (forM)
import           Foreign.Marshal.Alloc (malloc, mallocBytes)
import           Foreign.Marshal.Array (mallocArray)
import           Foreign.Ptr           (Ptr (..), nullPtr, plusPtr)
import           Foreign.Storable      (peek, peekElemOff, poke)
import           TreeSitter.Node
import           TreeSitter.Tree

getRootNode :: Ptr Tree -> IO Node
getRootNode ptr = getRootNodePtr ptr >>= getNodeFromPtr

getRootNodePtr :: Ptr Tree -> IO (Ptr Node)
getRootNodePtr tree = do
  n <- malloc
  ts_tree_root_node_p tree n
  return n

getNodeFromPtr :: Ptr Node -> IO Node
getNodeFromPtr ptr = do peek ptr

getChildrenFromNode :: TSNode -> Int -> IO (Ptr Node)
getChildrenFromNode nodeTSNode childCount = do
  children <- mallocArray childCount
  tsNode   <- malloc
  poke tsNode nodeTSNode
  ts_node_copy_child_nodes tsNode children
  return children

mapPtrTree :: (Node -> [a] -> IO a) -> Ptr Tree -> IO a
mapPtrTree f ptrTree = do
  rootNode <- getRootNode ptrTree
  mapNode f rootNode

mapNode :: (Node -> [a] -> IO a) -> Node -> IO a
mapNode f node = do
  let Node {..} = node

  let childCount = fromIntegral nodeChildCount
  children <- getChildrenFromNode nodeTSNode childCount
  children' <- mapChildren f children childCount

  f node children'

mapChildren :: (Node -> [a] -> IO a) -> Ptr Node -> Int -> IO [a]
mapChildren f children count =
  if count == 0
    then return []
    else forM
      [0 .. count - 1]
      (\n -> do
        child <- peekElemOff children n
        mapNode f child
      )
