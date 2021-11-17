{-# LANGUAGE InstanceSigs #-}
module Languages.While.Print where
import           Data.Foldable            (Foldable (fold))
import           Generics.Simplistic.Util (All)
import           Languages.While.Parse    (Stmt (..))

-- instance Show Stmt where
--   show :: Stmt -> String
--   show (Seq xs)              = foldMap show xs
--   show (Assign id expr)      = id ++ " = " ++ show expr ++ ";"
--   show (If expr stmt1 stmt2) = "if (" ++ show expr ++ ")\n" ++ "then " ++ show stmt1 ++ "\n" ++ "else " ++ show stmt2
--   show (While expr stmt)     = "while " ++ show expr ++ " do {\n" ++ show stmt ++ "}\n"
--   show Skip                  = ""
