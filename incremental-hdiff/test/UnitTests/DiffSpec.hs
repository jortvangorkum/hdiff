module UnitTests.DiffSpec where

import           CommandLine (Options (AST, Diff))
import           Lib         (mainAST, mainBody)
import           System.Exit (ExitCode (ExitSuccess))
import           Test.Hspec

spec :: Spec
spec = describe "Unit Tests" $ do
  it "Load in While file and get patch" $ do
    mainBody
      (Just "while")
      ( Diff
        "examples/While/Factorial/A.while"
        "examples/While/Factorial/O.while"
      )
      `shouldReturn` ExitSuccess

