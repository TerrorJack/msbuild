module MSBuild.Run
  ( runCommands
  , runCommandsWithX64NativeTools
  , compileX64CppStub
  ) where

import Control.Exception
import qualified Data.ByteString as BS
import Data.Foldable
import MSBuild.Query
import System.Directory
import System.FilePath
import System.IO
import System.Process

runCommands :: [String] -> IO ()
runCommands cmds =
  bracket
    (do tmpdir <- getTemporaryDirectory
        (p, h) <- openTempFile tmpdir "tmp.cmd"
        for_ cmds $ hPutStrLn h
        hClose h
        pure p)
    removeFile $
  callCommand . show

runCommandsWithX64NativeTools :: [String] -> IO ()
runCommandsWithX64NativeTools cmds = do
  p <- latestVCInstallationPath
  runCommands $
    ("call " ++ show (p </> "VC" </> "Auxiliary" </> "Build" </> "vcvars64.bat")) :
    cmds

compileX64CppStub :: BS.ByteString -> [String] -> IO FilePath
compileX64CppStub cpp args = do
  tmpdir <- getTemporaryDirectory
  bracket
    (do (p, h) <- openBinaryTempFile tmpdir "tmp.cpp"
        BS.hPut h cpp
        hClose h
        pure p)
    removeFile $ \cpp_p ->
    bracket
      (do (p, h) <- openBinaryTempFile tmpdir "tmp.obj"
          hClose h
          pure p)
      (\_ -> pure ()) $ \obj_p -> do
      runCommandsWithX64NativeTools
        [unwords $ ["cl.exe", "/c"] ++ args ++ ["/Fo:", show obj_p, show cpp_p]]
      pure obj_p
