{-# LANGUAGE RecordWildCards #-}
module Parse where

import           Control.Monad         (forM_)
import           Foreign.C.String
import           Foreign.Marshal.Alloc (malloc, mallocBytes)
import           Foreign.Marshal.Array (mallocArray)
import           Foreign.Marshal.Utils (new)
import           Foreign.Ptr           (Ptr (..), nullPtr, plusPtr)
import           Foreign.Storable      (peek, peekElemOff, poke)
import           TreeSitter.Haskell
import           TreeSitter.Node
import           TreeSitter.Parser
import           TreeSitter.Tree

parse :: IO ()
parse = do
  parser <- ts_parser_new
  ts_parser_set_language parser tree_sitter_haskell

  let sourceCode = "module Test (main) where\nimport Lib\nf1 = undefined\nf2 = undefined"

  (str, len) <- newCStringLen sourceCode
  tree <- ts_parser_parse_string parser nullPtr str len

  n <- malloc
  ts_tree_root_node_p tree n

  printChildren n 1

  return ()

printChildren :: Ptr Node -> Int -> IO ()
printChildren children count = forM_
  [0 .. count - 1]
  (\n -> do
    child <- peekElemOff children n
    printNode child
  )

printNode :: Node -> IO ()
printNode n@Node {..} = do
  theType <- peekCString nodeType
  let TSPoint {..} = nodeStartPoint n
      start        = "(" ++ show pointRow ++ "," ++ show pointColumn ++ ")"
  let TSPoint {..} = nodeEndPoint
      end          = "(" ++ show pointRow ++ "," ++ show pointColumn ++ ")"
  print $ theType ++ start ++ "-" ++ end
