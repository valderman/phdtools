import Text.Domplate
import System.Environment
import System.Directory
import System.Hclip
import qualified Data.ByteString as BS
import Data.ByteString.UTF8 (fromString, toString)
import Control.Monad
import System.Console.GetOpt
import Data.Char (isSpace)

opts :: [OptDescr (Cfg -> Cfg)]
opts = [
    Option "f"  ["file"] (ReqArg (\f c -> c {cfgFile = f}) "FILE")
      "Read context from FILE.",
    Option "h?" ["help"] (NoArg $ \c -> c {cfgPrintHelp = True})
      "Show this message.",
    Option "m"  ["manual-tags"] (NoArg $ \c -> c {cfgAutoTags = False})
      (unlines ["Don't insert replacement tags.",
               "The user is expected to do it manually.",
               "Template should follow the Domplate format."]),
    Option "n"  ["no-newlines"] (NoArg $ \c -> c {cfgInsertNewlines = False})
      "Don't insert newlines between responses."
  ]

usageHeader :: String
usageHeader = unlines [
    "Stitch together a text using a template and a context given by a YAML" ++
    " file.",
    "The template is given on the command line as follows:",
    "",
    "    $ reply RESPONSE1 RESPONSE2 ...",
    "",
    "This will take the text associated with RESPONSE1 and RESPONSE2, stitch",
    "them together separated by a newline, and save the result to the",
    "clipboard.",
    "",
    "The program's output may be modified using the following options:"
  ]

data Cfg = Cfg {
    -- | YAML file containing the canned responses.
    --   Default: @"responses.yaml"@
    cfgFile :: FilePath,

    -- | Print a help message instead of doing anything productive?
    --   Default: @False@
    cfgPrintHelp :: Bool,

    -- | Insert newlines between responses?
    --   Default: True
    cfgInsertNewlines :: Bool,

    -- | Insert @<div replace="arg"></div>@ for each argument?
    --   Default: True
    cfgAutoTags :: Bool
  }

-- | Default config.
defCfg :: Cfg
defCfg = Cfg {
    cfgFile = "responses.yaml",
    cfgPrintHelp = False,
    cfgInsertNewlines = True,
    cfgAutoTags = True
  }

-- | Build a template from a list of canned response identifiers.
mkTemplate :: Cfg -> [String] -> Template
mkTemplate cfg =
    parseTemplate
    . fromString
    . merge
    . replace
  where
    merge
      | cfgInsertNewlines cfg = unlines
      | otherwise             = concat
    replace
      | cfgAutoTags cfg = map (\i -> "<div replace=\"" ++ i ++ "\"></div>")
      | otherwise       = id

main = do
  (cfgs, args, errs) <- getOpt RequireOrder opts `fmap` getArgs
  let cfg = foldr (.) id cfgs defCfg
  case (cfgPrintHelp cfg, errs) of
    (True, _) -> do
      putStrLn $ usageInfo usageHeader opts
    (_, _:_) -> do
      fail . strip $ unlines errs
    (_, []) -> do
      isfile <- doesFileExist $ cfgFile cfg
      when (not isfile) $ fail $ "File '" ++ cfgFile cfg ++ "' does not exist."
      ctxfile <- BS.readFile $ cfgFile cfg

      case parseContext ctxfile >>= replace (mkTemplate cfg args) of
        Left err -> fail err
        Right s  -> setClipboard $ toString s

strip :: String -> String
strip = reverse . stripL . reverse . stripL
  where
    stripL = dropWhile isSpace
