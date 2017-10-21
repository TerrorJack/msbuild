{-# LANGUAGE TemplateHaskell #-}

module MSBuild.Query
  ( queryVSWherePath
  , runVSWhereWith
  , Entry(..)
  , queryVSEntries
  , latestMSBuildPath
  ) where

import Control.Exception
import Data.Aeson
import Data.Aeson.TH
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import Data.Time
import Data.Version
import Paths_msbuild
import System.Exit
import System.FilePath
import System.Process.ByteString.Lazy

queryVSWherePath :: IO FilePath
queryVSWherePath = do
  datadir <- getDataDir
  pure $ datadir </> "utils" </> "vswhere.exe"

runVSWhereWith :: FromJSON a => [String] -> IO a
runVSWhereWith args = do
  vswhere <- queryVSWherePath
  (c, o, _) <-
    readProcessWithExitCode vswhere (args ++ ["-format", "json"]) LBS.empty
  case c of
    ExitSuccess ->
      case eitherDecode' o of
        Left err -> fail err
        Right r -> pure r
    _ -> throwIO c

data Entry = Entry
  { instanceId :: !T.Text
  , installDate :: !UTCTime
  , installationName :: !T.Text
  , installationPath :: !FilePath
  , installationVersion :: !Version
  , productId :: !T.Text
  , productPath :: !FilePath
  , isPrerelease :: !Bool
  , displayName :: !T.Text
  , description :: !T.Text
  , channelId :: !T.Text
  , channelPath :: !FilePath
  , channelUri :: !String
  , enginePath :: !FilePath
  , releaseNotes :: !String
  , thirdPartyNotices :: !String
  , catalog :: !Object
  , properties :: !Object
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
