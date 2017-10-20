{-# OPTIONS -Wall #-}

import Data.Binary
import Distribution.Simple
import Distribution.Simple.LocalBuildInfo
import System.FilePath

main :: IO ()
main =
  defaultMainWithHooks
    simpleUserHooks
    { postConf =
        \args flags pkg_descr lbi -> do
          encodeFile ".buildinfo" $
            datadir (absoluteInstallDirs pkg_descr lbi NoCopyDest) </> "utils" </>
            "vswhere.exe"
          postConf simpleUserHooks args flags pkg_descr lbi
    }
