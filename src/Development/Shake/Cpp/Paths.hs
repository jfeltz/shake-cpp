module Development.Shake.Cpp.Paths where
import Development.Shake.FilePath 
import qualified Development.Shake.Iso as I

data BuildPaths = BuildPaths {
  outputPfx    :: FilePath,
  sourceObj    :: I.Iso, -- ^ source objects
  testObj      :: I.Iso, -- ^ test objects
  testExec     :: I.Iso, -- ^ test executables
  testStates   :: I.Iso,  -- ^ stores test pass states
  archives     :: FilePath  -- ^ stores custom archive targets
}

archivePath :: BuildPaths -> FilePath
archivePath bp = outputPfx bp </> archives bp 

data Paths = Paths {
  mainName     :: String,
  testSfx      :: String, 

  -- ^ e.g. "_test in source_test.cpp"
  -- this allows one to pattern match without
  -- regard to test name

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
    testSfx      = "_test",
    buildPaths   = defaultBuildPaths build_par
    }

defaultBuildPaths :: FilePath -> BuildPaths 
defaultBuildPaths build_par =  
  BuildPaths {  
    outputPfx   = build_par, 
    sourceObj   = I.Iso "src"   "bin", 
    testObj     = I.Iso "tests" "tests", 
    testExec    = I.Iso (build_par </> "tests") "test-bin",
    testStates  = I.Iso (build_par </> "test-bin") "test-state",
    archives    = "lib"
   }
