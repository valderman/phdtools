{-# LANGUAGE OverloadedStrings, DeriveDataTypeable,
             GeneralizedNewtypeDeriving #-}
-- | Reads and manipulates Crepes' environment.
module Crepes.Environment where
import Control.Applicative
import Control.Monad
import Control.Exception
import System.Directory
import System.FilePath
import System.IO.Unsafe
import System.Exit
import Database.SQLite.Simple
import Data.Version
import Data.Typeable

newtype CrepesException = CE String
  deriving (Typeable, Show)
instance Exception CrepesException

version :: Version
version = Version [0,1] []

versionString :: String
versionString = showVersion version

-- | Path to app data directory.
{-# NOINLINE appDir #-}
appDir :: FilePath
appDir = unsafePerformIO $ do
  dir <- getAppUserDataDirectory "crepes"
  d <- doesDirectoryExist dir
  when (not d) $ createDirectory dir
  return dir

dbFile :: FilePath
dbFile = appDir </> "timereport.sqlite"

defaultProjectFile :: FilePath
defaultProjectFile = ".crepes_default"

-- | Perform an action with an SQLite connection.
withSQLite :: FilePath -> (Connection -> IO a) -> IO a
withSQLite fp m = do
  f <- doesFileExist fp
  conn <- open fp
  when (not f) $ initialize conn
  m conn <* close conn

-- | Get the default project for this directory. A directory has a default
--   project if it contains a file called .crepes_default with a valid
--   project name.
getDefaultProject :: IO (Maybe String)
getDefaultProject = do
  f <- doesFileExist defaultProjectFile
  case f of
    False -> return Nothing
    _ -> withSQLite dbFile $ \c -> do
      proj <- readFile defaultProjectFile
      proj' <- query c "SELECT name FROM projects WHERE name = ?" (Only proj)
      case proj' of
        [Only s] | s == proj -> return (Just proj) -- fix the type
        _                    -> return Nothing

-- | Initialize the database.
initialize :: Connection -> IO ()
initialize c = do
  execute_ c $ "CREATE TABLE projects \
               \(id INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL,\
               \name text UNIQUE NOT NULL,\
               \quota DOUBLE);"
  execute_ c $ "CREATE TABLE cats \
               \(id INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL,\
               \name text UNIQUE NOT NULL);"
  execute_ c $ "CREATE TABLE time (pid   INTEGER,\
                                  \cat   INTEGER,\
                                  \date  DATE,\
                                  \hours DOUBLE NOT NULL,\
                                  \FOREIGN KEY(pid) REFERENCES projects(id),\
                                  \FOREIGN KEY(cat) REFERENCES cats(id));"
  execute_ c "INSERT INTO cats VALUES (0, 'uncategorized')"

-- | Print an error, then exit.
crepesError :: String -> IO a
crepesError = throw . CE
