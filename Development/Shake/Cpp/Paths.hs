-- FIXME testLib or testObjects?
module Development.Shake.Cpp.Paths where
import Development.Shake.FilePath 
import qualified Data.List as L 

-- | Invariant: input directory is single path element
data Pair = Pair { 
  input :: FilePath,
  output :: FilePath 
  } 

morphLeft :: BuildPaths -> String -> (BuildPaths -> Pair) -> FilePath -> FilePath 
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
  sourceObj    :: Pair, -- ^ source objects
  testObj      :: Pair, -- ^ test objects
  testExec     :: Pair, -- ^ test executables
  testStates   :: Pair,  -- ^ stores test pass states
  archives     :: FilePath  -- ^ stores custom archive targets
}

testLibPath :: BuildPaths -> FilePath
testLibPath bp = outputPfx bp </> testLib bp 

archivePath :: BuildPaths -> FilePath
archivePath bp = outputPfx bp </> archives bp 

outputPath :: (BuildPaths -> Pair) -> BuildPaths -> FilePath 
outputPath f bp = outputPfx bp </> (output . f $ bp)

inputPath :: (BuildPaths -> Pair) -> BuildPaths -> FilePath 
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
    sourceObj   = Pair "src"   "bin", 
    testObj     = Pair "tests" "tests", 
    testExec    = Pair (build_par </> "tests") "test-bin",
    testStates  = Pair (build_par </> "test-bin") "test-state",
    archives    = "lib"
   }
