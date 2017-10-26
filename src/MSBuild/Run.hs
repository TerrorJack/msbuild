module MSBuild.Run
  ( withTempFile
  , runCommands
  , runCommandsWithX64NativeTools
  , compileX64CppStub
  , linkX64ObjsToDLL
  ) where

import Control.Exception.Safe
import Control.Monad
import qualified Data.ByteString as BS
import Data.Foldable
import MSBuild.Query
import System.Directory
import System.FilePath
import System.IO
import System.Process

withTempFile :: Bool -> FilePath -> (FilePath -> IO r) -> IO r
withTempFile sweep pat cont = do
  tmpdir <- getTemporaryDirectory
  bracket
    (do (p, h) <- openBinaryTempFile tmpdir pat
        hClose h
        pure p)
    (when sweep . removeFile)
    cont

runCommands :: [String] -> IO ()
runCommands cmds =
  withTempFile True "tmp.cmd" $ \p -> do
    h <- openFile p WriteMode
    for_ cmds $ hPutStrLn h
    hClose h
    callCommand $ show p

runCommandsWithX64NativeTools :: [String] -> IO ()
runCommandsWithX64NativeTools cmds = do
  p <- latestVCInstallationPath
  runCommands $
    ("call " ++ show (p </> "VC" </> "Auxiliary" </> "Build" </> "vcvars64.bat")) :
    cmds

compileX64CppStub :: BS.ByteString -> [String] -> IO FilePath
compileX64CppStub cpp args =
  withTempFile True "tmp.cpp" $ \cpp_p -> do
    BS.writeFile cpp_p cpp
    withTempFile False "tmp.obj" $ \obj_p -> do
      runCommandsWithX64NativeTools
        [unwords $ ["cl.exe", "/c"] ++ args ++ ["/Fo:", show obj_p, show cpp_p]]
      pure obj_p

linkX64ObjsToDLL :: [FilePath] -> IO FilePath
linkX64ObjsToDLL objs =
  withTempFile False "tmp.dll" $ \dll_p -> do
    runCommandsWithX64NativeTools
      [unwords $ ["link.exe", "/DLL", "/OUT:" ++ show dll_p] ++ map show objs]
    pure dll_p
