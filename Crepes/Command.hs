{-# LANGUAGE GADTs, GeneralizedNewtypeDeriving #-}
-- | Commands describing what exactly a given run is supposed to do.
module Crepes.Command (
    Command (..), Year (..), Day (..), Date (..), ProjectName, Cat,
    Quota,
    parseCommand
  ) where
import Crepes.Parser
import Control.Applicative
import Data.Char

type ProjectName = String
type Cat = String
type Quota = Double

newtype Year = Year Int deriving (Eq, Ord, Num)
data Month =
  Jan | Feb | Mar | Apr | May | Jun | Jul | Aug | Sep | Oct | Nov | Dec
  deriving (Eq, Ord, Show, Enum)
newtype Day = Day Int deriving (Eq, Ord, Num)
data Date = Date Year Month Day deriving (Eq, Ord)

instance Show Date where
  show (Date (Year y) m (Day d)) =
    show y ++ "-" ++ pad 2 (fromEnum m+1) ++ "-" ++ pad 2 d

pad :: Int -> Int -> String
pad n x = replicate (n - length s) ' ' ++ s
  where s = show x

prettyDate :: Date -> String
prettyDate (Date (Year y) m (Day d)) = show d ++ " " ++ show m ++ " " ++ show y

data Command where
  Create   :: ProjectName -> Maybe Quota -> Command
  Delete   :: ProjectName -> Command
  Projects :: Command
  SetQuota :: ProjectName -> Maybe Quota -> Command
  AddCat   :: Cat -> Command
  DelCat   :: Cat -> Command
  ListCats :: Command
  AddTime  :: ProjectName -> Double -> Maybe Cat -> Maybe Date -> Command
  Report   :: Maybe ProjectName -> Command
  Def      :: Maybe ProjectName -> Command
  Help     :: Command

-- | Parse a command. References to projects and categories are still
--   unresolved.
parseCommand :: Maybe String -> String -> Maybe Command
parseCommand defaultProject =
    runParser cmd
  where
    space = whitespace
    
    project =
      case defaultProject of
        Just dp -> oneOf [identifier, return dp]
        _       -> identifier
    
    cmd = do
      c <- oneOf [string "create"    >> space >> create,
                  string "delete"    >> space >> fmap Delete project,
                  string "projects"  >> return Projects,
                  string "newcat"    >> space >> fmap AddCat project,
                  string "delcat"    >> space >> fmap DelCat project,
                  string "cats"      >> return ListCats,
                  string "report"    >> space >> fmap (Report . Just) project,
                  string "quota"     >> space >> setQuota,
                  string "noquota"   >> space >> noQuota,
                  string "default"   >> space >> fmap (Def . Just) project,
                  string "nodefault" >> return (Def Nothing),
                  string "help"      >> return Help,
                  addTime]
      _ <- whitespace
      return c
    
    identifier = oneOf [quotedString '"',
                        quotedString '\'',
                        word `suchThat` (not . isDigit . head)]
    
    -- quota command
    setQuota = do
      name <- project <* whitespace
      quota <- double
      return $ SetQuota name (Just quota)

    -- noquota command
    noQuota = do
      name <- project <* whitespace
      return $ SetQuota name Nothing

    -- Parse a create command
    create = do
      name <- identifier <* whitespace
      quota <- possibly double
      return $ Create name quota
    
    -- Parse an add time command
    addTime = do
      proj <- project <* whitespace
      time <- double  <* whitespace
      oneOf [do cat <- word <* whitespace
                d <- date
                return $ AddTime proj time (Just cat) (Just d),
             do d <- date <* whitespace
                return $ AddTime proj time Nothing (Just d),
             do cat <- word
                return $ AddTime proj time (Just cat) Nothing,
             do return $ AddTime proj time Nothing Nothing]
    
    -- Dates are entered in yyyy-mm-dd format.
    date = do
      year <- Year <$> int
      _ <- char '-'
      month <- toEnum . pred <$> int `suchThat` (\x -> x > 0 && x <= 12)
      _ <- char '-'
      day <- Day <$> int `suchThat` (\x -> x > 0 && x <= 31)
      return $ Date year month day
