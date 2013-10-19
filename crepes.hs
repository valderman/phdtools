module Main where
import Control.Applicative
import System.Environment
import Crepes.Command
import Crepes.Runner
import Crepes.Environment
import Crepes.REPL

main = do
  defp <- getDefaultProject
  args <- getArgs
  case args of
    [] -> repl
    _  -> do
      mcmd <- parseCommand defp . unwords <$> getArgs
      case mcmd of
        Just cmd -> withSQLite dbFile $ flip performCommand cmd
        _        -> crepesError "Invalid command; try 'crepes help'"
