module Parse where

import           Foreign.C.String
import           Foreign.Ptr
import           Parse.Print
import           Parse.Tree         (convertPtrToTree)
import           TreeSitter.Haskell
import           TreeSitter.Parser
import           TreeSitter.Tree

getParseTreeFromFile :: Ptr Parser -> String -> IO (Ptr Tree)
getParseTreeFromFile parser path = do
  sourceCode <- readFile path
  (str, len) <- newCStringLen sourceCode
  ts_parser_parse_string parser nullPtr str len

parse :: IO ()
parse = do
  parser <- ts_parser_new
  ts_parser_set_language parser tree_sitter_haskell

  treeO <- getParseTreeFromFile parser "./examples/While/Big/100/O.hs"

  -- printTree tree

  -- pTree <- convertPtrToTree tree

  -- print pTree

  return ()
