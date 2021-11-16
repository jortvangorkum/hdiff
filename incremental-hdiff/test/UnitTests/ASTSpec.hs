module UnitTests.ASTSpec where

import           CommandLine     (Options (AST))
import           Lib             (mainAST)
import           System.Exit     (ExitCode (ExitSuccess))
import           Test.Hspec
import           Test.QuickCheck

spec :: Spec
spec = describe "Unit Tests" $ do
  it "Load in While file and parse to AST" $ do
    mainAST (Just "while") (AST "examples/While/Factorial/A.while") `shouldReturn` ExitSuccess

