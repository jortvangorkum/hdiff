module UnitTests.ASTSpec where

import           CommandLine (Options (AST))
import           Lib         (mainBody)
import           System.Exit (ExitCode (ExitSuccess))
import           Test.Hspec

spec :: Spec
spec = describe "Unit Tests" $ do
  it "Load in While file and parse to AST" $ do
    mainBody (Just "while") (AST "examples/While/Factorial/A.while") `shouldReturn` ExitSuccess

