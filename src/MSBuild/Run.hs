module MSBuild.Run
  ( runCommandsWithBat
  , runCommandsWithX64NativeTools
  ) where

import Data.List
import MSBuild.Where
import System.FilePath
import System.Process

runCommandsWithBat :: FilePath -> [String] -> IO ()
runCommandsWithBat bat cmds = callCommand $ intercalate " & " (show bat : cmds)

runCommandsWithX64NativeTools :: [String] -> IO ()
runCommandsWithX64NativeTools cmds = do
  p <- latestMSBuildPath
  runCommandsWithBat
    (p </> "VC" </> "Auxiliary" </> "Build" </> "vcvars64.bat")
    cmds
