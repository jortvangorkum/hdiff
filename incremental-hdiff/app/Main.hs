module Main where
import           CommandLine         (cmdOptions)
import           GHC.Real            (fromIntegral)
import           Lib                 (mainBody)
import           Options.Applicative (execParser)
import           System.CPUTime      (getCPUTime)
import           System.Exit         (exitWith)
import           Text.Printf         (printf)
import           Tree

-- main :: IO ()
-- main = do
--   options <- execParser cmdOptions
--   exitCode <- mainBody (Just "while") options
--   exitWith exitCode

main :: IO ()
main = do
  let x = generateTree 21
  let y = changeLeaf 100 $ generateTree 21



  return ()
