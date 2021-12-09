module Parse where

import           Foreign.C.String
import           Foreign.Ptr
import           Parse.Print
import           Parse.Tree         (convertPtrToTree)
import           TreeSitter.Haskell
import           TreeSitter.Parser

parse :: IO ()
parse = do
  parser <- ts_parser_new
  ts_parser_set_language parser tree_sitter_haskell

  let sourceCode = "module Test (main) where\nimport Lib\nf1 = undefined\nf2 = undefined"

  (str, len) <- newCStringLen sourceCode
  tree <- ts_parser_parse_string parser nullPtr str len

  printTree tree

  pTree <- convertPtrToTree tree

  print pTree

  return ()
