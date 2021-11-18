module Languages.Main where
import           Languages.Interface   (LangParser (..))
import qualified Languages.While.Parse as While

mainParsers :: [LangParser]
mainParsers = [LangParser "while" (fmap While.dfromWhile . While.parseFile)]
