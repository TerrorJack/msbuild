{-# LANGUAGE OverloadedStrings #-}

import MSBuild.Run
import System.Directory

main :: IO ()
main = do
  p <-
    compileX64CppStub
      "extern \"C\" { int plus(int x, int y) { return x + y; } }"
      []
  removeFile p
