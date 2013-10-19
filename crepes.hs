module Main where
import Control.Applicative
import Control.Exception
import System.Environment
import System.Exit
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
        Just cmd -> catch (withSQLite dbFile $ flip performCommand cmd) $ \(CE err) -> do
                      putStrLn err >> exitFailure
        _        -> putStrLn "Invalid command; try 'crepes help'" >> exitFailure
