-- FIXME testLib or testObjects?
module Development.Shake.Cpp.Paths where
import Development.Shake.FilePath 
import qualified Data.List as L 

data Iso = Iso { 
  input :: FilePath,
  output :: FilePath 
  } 

morphLeft :: BuildPaths -> String -> (BuildPaths -> Iso) -> FilePath -> FilePath 
morphLeft bp in_ext f output_path = 
  input (f bp) </> dropped_output -<.> in_ext 
  where
    -- | Drop the full output prefix of the output file path
    dropped_output :: FilePath
    dropped_output = 
      joinPath $ 
        drop 
          (L.length $ splitPath (outputPfx bp </> output (f bp))) 
          (splitPath output_path) 

data BuildPaths = BuildPaths {
  outputPfx    :: FilePath,
  testLib      :: FilePath, 
  sourceObj    :: Iso, -- ^ source objects
  testObj      :: Iso, -- ^ test objects
  testExec     :: Iso, -- ^ test executables
  testStates   :: Iso,  -- ^ stores test pass states
  archives     :: FilePath  -- ^ stores custom archive targets
}

testLibPath :: BuildPaths -> FilePath
testLibPath bp = outputPfx bp </> testLib bp 

archivePath :: BuildPaths -> FilePath
archivePath bp = outputPfx bp </> archives bp 

outputPath :: (BuildPaths -> Iso) -> BuildPaths -> FilePath 
outputPath f bp = outputPfx bp </> (output . f $ bp)

inputPath :: (BuildPaths -> Iso) -> BuildPaths -> FilePath 
inputPath f = input . f

data Paths = Paths {
  mainName     :: String,
  testPfx      :: String, -- ^ e.g. "test_ in test_source.cpp"
  cppExtension :: String, -- ^ e.g. "cpp"
  objExtension :: String, -- ^ e.g. "o"
  tsExtension  :: String, -- ^ e.g. "pass"
  buildPaths   :: BuildPaths
  }

defaultPaths  :: FilePath -> Paths 
defaultPaths build_par =
  Paths {
    mainName     = "main", 
    cppExtension = "cc", 
    objExtension = "o", 
    tsExtension  = "pass", 
    testPfx      = "test_",
    buildPaths   = defaultBuildPaths build_par
    }

defaultBuildPaths :: FilePath -> BuildPaths 
defaultBuildPaths build_par =  
  BuildPaths {  
    outputPfx   = build_par, 
    testLib     = "test-lib",
    sourceObj   = Iso "src"   "bin", 
    testObj     = Iso "tests" "tests", 
    testExec    = Iso (build_par </> "tests") "test-bin",
    testStates  = Iso (build_par </> "test-bin") "test-state",
    archives    = "lib"
   }
