{-# LANGUAGE TemplateHaskell #-}

module MSBuild.Internals
  ( vcswherePath
  , runVcswhereWith
  ) where

import Control.Exception
import Data.Aeson
import Data.Binary
import qualified Data.ByteString.Lazy as LBS
import Language.Haskell.TH.Syntax
import System.Exit
import System.Process.ByteString.Lazy

vcswherePath :: FilePath
vcswherePath =
  $(do p <- runIO $ decodeFile ".buildinfo"
       lift (p :: FilePath))

runVcswhereWith :: FromJSON a => [String] -> IO a
runVcswhereWith args = do
  (c, o, _) <- readProcessWithExitCode vcswherePath args LBS.empty
  case c of
    ExitSuccess ->
      case eitherDecode' o of
        Left err -> fail err
        Right r -> pure r
    _ -> throwIO c
