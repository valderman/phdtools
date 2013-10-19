-- | Really basic REPL - for when you have a lot of timekeeping to do.
module Crepes.REPL where
import System.Console.Haskeline
import Control.Monad.IO.Class
import Crepes.Runner
import Crepes.Command
import Crepes.Environment
import Crepes.Parser

repl :: IO ()
repl = do
    putStrLn $ "crepes REPL " ++ versionString
    putStrLn $ "Type 'help' for help, 'quit' to exit."
    withSQLite dbFile $ runInputT defaultSettings . go
  where
    go c = flip catch justQuit $ do
      s <- getInputLine "> "
      case fmap (runParser exitWords) s of
        Just Nothing -> do
          defProj <- liftIO getDefaultProject
          case s >>= parseCommand defProj of
            Nothing  -> outputStrLn "Invalid command; try 'help'"
            Just cmd -> liftIO $ performCommand c cmd
          go c
        _ -> do
          justQuit undefined

    justQuit :: SomeException -> InputT IO ()
    justQuit _ = outputStrLn "bye!"

    exitWords = do
      _ <- whitespace
      oneOf [string "exit", string "quit", string "bye"]
      _ <- whitespace
      return ()
