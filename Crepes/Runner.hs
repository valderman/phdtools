{-# LANGUAGE OverloadedStrings #-}
-- | Runs Crepes.Command commands.
module Crepes.Runner (performCommand) where
import Control.Exception
import Control.Applicative
import Control.Monad
import System.Directory
import System.IO
import Data.Time.Clock
import Data.Time.Calendar
import Data.Char
import Data.List
import Database.SQLite.Simple
import Crepes.Command
import Crepes.Environment

-- | Execute a command over the database.
performCommand :: Connection -> Command -> IO ()
performCommand c cmd =
    go cmd
  where
    -- Suddenly, commands, hundreds of them!
    go (Create name quota) = flip catch (createError name) $ do
      execute c "INSERT INTO projects (name,quota) VALUES (?,?)" (name, quota)
      putStrLn "ok"

    go (Delete name) = do
      putStrLn $ "Are you sure you want to PERMANENTLY delete the project '"
               ++ name ++ "'?"
      putStrLn $ "All time logged for the project will be lost."
      confirm $ execute c "DELETE FROM projects WHERE name = ?" (Only name)

    go (Projects) = do
      q <- query_ c "SELECT name FROM projects"
      mapM_ (putStrLn . head) q

    go (SetQuota name quota) = do
      execute c "UPDATE projects SET quota = ? WHERE name = ?" (quota, name)
      putStrLn "ok"

    go (AddCat cat) = flip catch (createCatError cat) $ do
      execute c "INSERT INTO cats (name) VALUES (?)" (Only cat)
      putStrLn "ok"

    go (DelCat cat) = do
      putStrLn $ "Are you sure you want to PERMANENTLY delete the category '"
               ++ cat ++ "'?"
      putStrLn $ "All time logged for the category will become uncategorized."
      confirm $ flip catch (noSuchCat cat) $ do
        [[cid]] <- query c "SELECT id FROM cats WHERE name = ?" (Only cat)
        execute c "UPDATE time SET cat = 0 WHERE cat = ?" (Only (cid::Int))
        execute c "DELETE FROM cats WHERE name = ?" (Only cat)

    go (ListCats) = do
      q <- query_ c "SELECT name FROM cats"
      mapM_ (putStrLn . head) q

    go (AddTime proj hours mcat mdate) = do
      addTime c proj hours mcat mdate

    go (Report (Just proj)) = do
      projReport c proj
    
    go (Def (Just proj)) = do
      p <- query c "SELECT name FROM projects WHERE name = ?" (Only proj)
      case p of
        [Only p'] | p' == proj -> do
          writeFile defaultProjectFile proj
          putStrLn "ok"
        _ ->
          noSuchProj proj undefined
    
    go (Def Nothing) = do
      f <- doesFileExist defaultProjectFile
      when f $ removeFile defaultProjectFile
      putStrLn "ok"
    
    go (Help) = do
      putStrLn "crepes commands:"
      putStrLn "  create <project> [quota]"
      putStrLn "  delete <project>"
      putStrLn "  projects"
      putStrLn "  newcat <cat>"
      putStrLn "  delcat <cat>"
      putStrLn "  cats"
      putStrLn "  report <project>"
      putStrLn "  quota <project> <hours>"
      putStrLn "  noquota <project>"
      putStrLn "  default <project>"
      putStrLn "  nodefault"
      putStrLn "  help"
      putStrLn "  <project> <hours> [cat] [yyyy-mm-dd]"
      putStrLn ""
      putStrLn "Parameters in angle brackets are mandatory; parameters in\n\
               \normal brackets are optional.\n"
      putStrLn "The <project> parameter may be omitted if you are calling\n\
               \crepes from a directory where you have previously ran\n\
               \'crepes default <project>' to set a default project.\n"

-- | Add a time entry to a project, possibly with cat and date info.
--   TODO: warnings when getting close to quota, etc.
addTime :: Connection -> ProjectName -> Double -> Maybe Cat -> Maybe Date -> IO ()
addTime c proj hours (Just cat) (Just date) = do
  pid <- flip catch (\e -> noSuchProj proj e >> return (0::Int)) $ do
    [[pid]] <- query c "SELECT id FROM projects WHERE name = ?" (Only proj)
    return pid
  cid <- flip catch (\e -> noSuchCat cat e >> return (0::Int)) $ do
    [[cid]] <- query c "SELECT id FROM cats WHERE name = ?" (Only cat)
    return cid
  execute c "INSERT INTO time VALUES (?,?,?,?)" (pid, cid, show date, hours)
  quotahrs <- query c "SELECT MIN(p.quota), SUM(t.hours) FROM time AS t \
                      \JOIN projects AS p ON p.id = t.pid \
                      \WHERE p.id = ?" (Only pid) :: IO [(Double, Double)]
  putStr "ok"
  case quotahrs of
    [(quota, hours)] -> do
      putStrLn $ "; " ++ show hours ++ " hours used out of " ++ show quota ++
                 " (" ++ show (round (100*(hours/quota))) ++ "%)"
    _ -> do
      putStrLn ""
addTime c proj hours Nothing mdate = do
  -- Kind of silly, roundabout way of looking up the uncategorized category...
  cat <- head . head <$> query_ c "SELECT name FROM cats WHERE id = 0"
  addTime c proj hours (Just cat) mdate
addTime c proj hours mcat Nothing = do
  (y, m, d) <- toGregorian . utctDay <$> getCurrentTime  
  let y' = fromInteger y
      m' = toEnum (m-1)
  addTime c proj hours mcat (Just $ Date (Year y') m' (Day d))

-- | Generate and print a project report.
projReport :: Connection -> ProjectName -> IO ()
projReport c proj = do
  byCat <- flip (query c) (Only proj)
    "SELECT c.name, SUM(t.hours) FROM time AS t \
    \JOIN projects AS p ON p.id = t.pid \
    \JOIN cats AS c ON c.id = t.cat \
    \WHERE p.name = ? \
    \GROUP BY t.cat" :: IO [(String, Double)]
  [[quota]] <- query c "SELECT quota FROM projects WHERE name = ?" (Only proj)
  printReport quota proj byCat

-- | Print a report for a single project.
printReport :: Maybe Quota -> String -> [(String, Double)] -> IO ()
printReport mquota proj cats = do
    putStrLn $ "** General report for " ++ proj ++ " **"
    putStrLn ""
    case mquota of
      Just quota -> do
        putStrLn "Hours allotted:"
        putStrLn $ "  " ++ show quota
        putStrLn ""
        byCat
        putStrLn "Hours left:"
        putStrLn $ "  " ++ show (quota - total)
        putStrLn ""
      _ ->
        byCat
  where
    byCat = do
      putStrLn $ "Hours spent by category:"
      mapM_ (printCat . addPadding) (sortBy mostHours cats)
      putStrLn $ "  " ++ replicate padding '-'
      putStrLn $ "  " ++ padRight padding "total"
                      ++ padLeft hrspadding (show total)
      putStrLn ""

    printCat (cat, hrs, hrs') =
      putStrLn $ "  " ++ cat ++ hrs' ++ percentage hrs
    total = sum $ map snd cats
    padding = maximum (map (length . fst) cats) + 4
    hrspadding = maximum (map (length . show . snd) cats)
    padRight n s = s ++ replicate (n-length s) ' '
    padLeft n s = replicate (n-length s) ' ' ++ s
    addPadding (cat, hrs) = (padRight padding cat,
                             hrs,
                             padLeft hrspadding (show hrs))
    percentage h = concat ["     ", show $ round $ 100*(h/total), "%"]
    mostHours (_, h1) (_, h2) = compare h2 h1

-- | Have the user confirm an action by typing in an annoying sentence.
confirm :: IO () -> IO ()
confirm act = do
  putStrLn $ "If you are sure, please type \"yes, I know what I'm doing\"" 
           ++ " without the quotes."
  putStr "> " >> hFlush stdout
  str <- getLine
  case map toLower str of
    "yes, i know what i'm doing" -> act <* putStrLn "ok"
    _                            -> putStrLn "aborting"

-- | More specific error messages.
createError, createCatError :: String -> SomeException -> IO ()
noSuchProj, noSuchCat :: String -> SomeException -> IO ()
createError name _ = crepesError $
  "Couldn't create project '" ++ name ++ "'; maybe it already exists?"
createCatError name _ = crepesError $
  "Couldn't create category '" ++ name ++ "'; maybe it already exists?"
noSuchProj name _ = crepesError $
  "Project '" ++ name ++ "' doesn't exist!"
noSuchCat name _ = crepesError $
  "Category '" ++ name ++ "' doesn't exist!"
