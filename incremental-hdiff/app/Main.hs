module Main where
import           CommandLine         (cmdOptions)
import           Lib                 (mainAST)
import           Options.Applicative (execParser)
import           System.Exit         (exitWith)

main :: IO ()
main = do
  options <- execParser cmdOptions
  exitCode <- mainAST (Just "while") options
  exitWith exitCode
