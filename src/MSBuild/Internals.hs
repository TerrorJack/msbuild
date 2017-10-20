{-# LANGUAGE TemplateHaskell #-}

module MSBuild.Internals
  ( vcswherePath
  ) where

import Data.Binary
import Language.Haskell.TH.Syntax

vcswherePath :: FilePath
vcswherePath =
  $(do p <- runIO $ decodeFile ".buildinfo"
       lift (p :: FilePath))
