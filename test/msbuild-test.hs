import MSBuild.Run

main :: IO ()
main = runCommandsWithX64NativeTools ["cl.exe"]
