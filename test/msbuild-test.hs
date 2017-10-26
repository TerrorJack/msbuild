{-# LANGUAGE OverloadedStrings #-}

import MSBuild.Query
import MSBuild.Run
import System.Directory
import Text.Show.Pretty

main :: IO ()
main = do
  entries <- queryEntries
  pPrint entries
  p <-
    compileX64CppStub
      "extern \"C\" { int plus(int x, int y) { return x + y; } }"
      ["/Ox"]
  removeFile p
