module Development.Shake.Cpp.Obj where
import Development.Shake.Cpp.Paths
import Development.Shake.FilePath

data Obj = Archive | Src | Test deriving (Ord, Eq)

-- | Return the parent location for an object type 
toLibBin :: Obj -> BuildPaths -> FilePath 
toLibBin Src     bp = outputPath sourceObj bp
toLibBin Archive bp = outputPfx bp </> archives bp 
toLibBin Test    bp = outputPath testObj bp 

-- | return a target directory for the exe
-- from the object type
toExeBin :: Obj -> BuildPaths -> FilePath 
toExeBin Archive = archivePath
toExeBin Src     = outputPath sourceObj
toExeBin Test    = outputPath testObj
