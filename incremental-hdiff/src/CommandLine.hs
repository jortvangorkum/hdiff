module CommandLine where
import           Options.Applicative

data Options
  = AST  { optFileA :: FilePath
         }
  | Diff { optFileA :: FilePath
         , optFileB :: FilePath
         }

astOpts :: Parser Options
astOpts = AST <$> strArgument (metavar "FILE")

diffOpts :: Parser Options
diffOpts =
  Diff <$> strArgument (metavar "OLDFILE")
       <*> strArgument (metavar "NEWFILE")

parseOptions :: Parser Options
parseOptions = subparser
  (  command "ast" (info astOpts ( progDesc "Parses and displays an AST"))
  <> command "diff" (info diffOpts (progDesc "Runs Diff on targets"))
  ) <|> diffOpts

cmdOptions :: ParserInfo Options
cmdOptions = info (parseOptions <**> helper)
  (  fullDesc
  <> progDesc  "Runs hdiff with specified command, 'diff' is the default command"
  )
