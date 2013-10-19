module Main where
import Control.Applicative
import System.Environment
import Crepes.Command
import Crepes.Runner
import Crepes.Environment

main = do
  defp <- getDefaultProject
  mcmd <- parseCommand defp . unwords <$> getArgs
  case mcmd of
    Just cmd -> withSQLite dbFile $ flip performCommand cmd
    _        -> crepesError "Invalid command; try 'crepes help'"
