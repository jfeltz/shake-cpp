module Development.Shake.Iso where
import Development.Shake.FilePath 
import qualified Data.List as L 

data Iso = Iso {
  input :: FilePath,
  output :: FilePath
  } 
  
-- | Convert an Output FilePath to an input FilePath 
morphLeft :: String -> FilePath -> Iso -> FilePath -> FilePath 
morphLeft ext output_pfx iso output_tgt = 
  input iso </> joinPath (L.drop dropped (splitPath output_tgt)) -<.> ext
  where
    dropped = L.length $ splitPath (output_pfx </> output iso)

morphRight :: String -> FilePath -> Iso -> FilePath -> FilePath 
morphRight ext output_pfx iso input_file =
  outputPath iso output_pfx </> input_file -<.> ext

outputPath :: Iso -> FilePath -> FilePath 
outputPath iso output_pfx = output_pfx </> output iso 

