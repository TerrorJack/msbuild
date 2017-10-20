{-# LANGUAGE TemplateHaskell #-}

module MSBuild.Where
  ( vswherePath
  , runVSWhereWith
  , Entry(..)
  , queryVSEntries
  , latestMSBuildPath
  ) where

import Control.Exception
import Data.Aeson
import Data.Aeson.TH
import Data.Binary
import qualified Data.ByteString.Lazy as LBS
import Language.Haskell.TH.Syntax
import System.Exit
import System.Process.ByteString.Lazy

vswherePath :: FilePath
vswherePath =
  $(do p <- runIO $ decodeFile ".buildinfo"
       lift (p :: FilePath))

runVSWhereWith :: FromJSON a => [String] -> IO a
runVSWhereWith args = do
  (c, o, _) <-
    readProcessWithExitCode vswherePath (args ++ ["-format", "json"]) LBS.empty
  case c of
    ExitSuccess ->
      case eitherDecode' o of
        Left err -> fail err
        Right r -> pure r
    _ -> throwIO c

data Entry = Entry
  { installationPath :: !FilePath
  , isPrerelease :: !Bool
  }

$(deriveFromJSON defaultOptions 'Entry)

queryVSEntries :: IO [Entry]
queryVSEntries = runVSWhereWith ["-products", "*"]

latestMSBuildPath :: IO FilePath
latestMSBuildPath = do
  r <- runVSWhereWith ["-products", "*", "-latest"]
  case r of
    [e] -> pure $ installationPath e
    _ ->
      fail $ "vswhere returned " ++ show (length r) ++ " results, expecting 1"