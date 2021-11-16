module Main where
import           CommandLine         (cmdOptions)
import           Lib                 (mainBody)
import           Options.Applicative (execParser)
import           System.Exit         (exitWith)

main :: IO ()
main = do
  options <- execParser cmdOptions
  exitCode <- mainBody (Just "while") options
  exitWith exitCode
