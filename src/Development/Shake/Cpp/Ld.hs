module Development.Shake.Cpp.Ld where
import Development.Shake
       
archive :: FilePath -> [FilePath] -> Action ()
archive = cmd "ld " "-r -o"
