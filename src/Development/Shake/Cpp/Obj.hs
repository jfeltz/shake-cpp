module Development.Shake.Cpp.Obj where
import Development.Shake.Cpp.Paths
import Development.Shake.FilePath
import Development.Shake.Iso

data Obj = Archive | Src | Test deriving (Ord, Eq)

-- | Return the parent location for an object type 
toLibBin :: Obj -> BuildPaths -> FilePath 
toLibBin Src     b = outputPath (sourceObj b) (outputPfx b)
toLibBin Archive b = outputPfx b </> archives b 
toLibBin Test    b = outputPath (testObj b) (outputPfx b)

-- | return a target directory for the exe
-- from the object type
toExeBin :: Obj -> BuildPaths -> FilePath 
toExeBin Archive b = archivePath b
toExeBin Src     b = outputPath (sourceObj b) (outputPfx b)
toExeBin Test    b = outputPath (testObj b) (outputPfx b)
