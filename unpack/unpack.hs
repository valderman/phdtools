{-# LANGUAGE OverloadedStrings #-}
import Control.Shell
import Control.Shell.Extract
import Data.List (intercalate)
import System.Console.GetOpt

data Flag
  = DeleteAfterExtract
  | PrintHelp
  | SeparateDirs
  | OutDir FilePath
    deriving (Eq, Show)

-- | Get the output directory from the last OutDir flag in a list.
--   Returns @"."@ if no such flag is found.
outdir :: [Flag] -> FilePath
outdir = go . reverse
  where
    go (OutDir dir : _) = dir
    go (_ : xs)         = go xs
    go _                = "."

opts :: [OptDescr Flag]
opts =
  [ Option "d" ["delete"] (NoArg DeleteAfterExtract) $
      "Delete archive(s) after extraction."
  , Option "o" ["outdir"] (ReqArg OutDir "DIR") $
      "Extract files into DIR."
  , Option "s" ["separate-dirs"] (NoArg SeparateDirs) $
      "Extract files into separate directories."
  , Option "?h" ["help"] (NoArg PrintHelp) $
      "Display this message."
  ]

helpText :: String
helpText = unlines
  [ "Usage: unpack [OPTIONS] FILE(s)"
  , "Extract a variety of different archive files."
  , "Supported formats: " ++ intercalate ", " supportedExtensions
  ]

main = shell_ $ do
  let (flags, archives, errors) = getOpt Permute opts cmdline
  -- Bail out on bad args
  when (not $ null errors) $ do
    mapM_ echo_ errors
    fail "failed to parse command-line options"

  -- Bail out on help message
  when (PrintHelp `elem` flags) $ do
    echo $ usageInfo helpText opts
    exit

  -- Otherwise, unpack everything.
  let exopts = defaultExtractOptions
        { separateDirectory = SeparateDirs `elem` flags
        , removeArchive     = DeleteAfterExtract `elem` flags
        }
  mapM_ (unpack exopts (outdir flags)) archives

-- | Unpack an archive.
unpack :: ExtractOptions -> FilePath -> FilePath -> Shell ()
unpack opts outdir archive = do
  dir <- pwd
  inDirectory outdir $ extractWith opts (dir </> archive)
