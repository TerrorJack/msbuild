{-# LANGUAGE OverloadedStrings #-}

import Control.Exception.Safe
import Foreign
import Foreign.C
import MSBuild.Query
import MSBuild.Run
import System.Directory
import System.Win32.DLL
import Text.Show.Pretty

autoRemove :: IO FilePath -> (FilePath -> IO r) -> IO r
autoRemove m = bracket m removeFile

foreign import ccall "dynamic" mkPlus ::
               FunPtr (CInt -> CInt -> IO CInt) -> CInt -> CInt -> IO CInt

main :: IO ()
main = do
  entries <- queryEntries
  pPrint entries
  autoRemove
    (compileX64CppStub
       "extern \"C\" __declspec(dllexport) int plus(int x, int y) { return x + y; }"
       ["/Ox"]) $ \obj_p ->
    autoRemove (linkX64ObjsToDLL [obj_p]) $ \dll_p -> do
      lib <- loadLibrary dll_p
      fptr <- getProcAddress lib "plus"
      r <- mkPlus (castPtrToFunPtr fptr) 2 3
      print r
      freeLibrary lib
