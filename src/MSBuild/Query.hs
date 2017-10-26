{-# LANGUAGE TemplateHaskell #-}

module MSBuild.Query
  ( vswherePath
  , runVSWhereWith
  , Entry(..)
  , queryEntries
  , latestVCInstallationPath
  ) where

import Control.Exception
import Data.Aeson
import Data.Aeson.TH
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import Data.Time
import Data.Version
import Language.Haskell.TH.Syntax
import Paths_msbuild
import System.Exit
import System.FilePath
import System.Process.ByteString.Lazy

vswherePath :: FilePath
vswherePath =
  $(do p <- runIO getDataDir
       lift $ p </> "utils" </> "vswhere.exe")

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
  } deriving (Show)

$(deriveFromJSON defaultOptions 'Entry)

queryEntries :: IO [Entry]
queryEntries = runVSWhereWith ["-products", "*"]

latestVCInstallationPath :: IO FilePath
latestVCInstallationPath = do
  r <-
    runVSWhereWith
      [ "-products"
      , "*"
      , "-requires"
      , "Microsoft.VisualStudio.Component.VC.Tools.x86.x64"
      , "-latest"
      ]
  case r of
    [e] -> pure $ installationPath e
    _ ->
      fail $ "vswhere returned " ++ show (length r) ++ " results, expecting 1"
