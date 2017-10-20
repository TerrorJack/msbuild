{-# LANGUAGE TemplateHaskell #-}

module MSBuild.Internals
  ( vswherePath
  , runVswhereWith
  ) where

import Control.Exception
import Data.Aeson
import Data.Binary
import qualified Data.ByteString.Lazy as LBS
import Language.Haskell.TH.Syntax
import System.Exit
import System.Process.ByteString.Lazy

vswherePath :: FilePath
vswherePath =
  $(do p <- runIO $ decodeFile ".buildinfo"
       lift (p :: FilePath))

runVswhereWith :: FromJSON a => [String] -> IO a
runVswhereWith args = do
  (c, o, _) <- readProcessWithExitCode vswherePath args LBS.empty
  case c of
    ExitSuccess ->
      case eitherDecode' o of
        Left err -> fail err
        Right r -> pure r
    _ -> throwIO c
