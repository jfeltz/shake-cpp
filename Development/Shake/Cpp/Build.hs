{-# LANGUAGE TupleSections #-}
module Development.Shake.Cpp.Build where
import           Control.Applicative ((<$>))
import           Control.Monad.Reader
import           Development.Shake
import           Development.Shake.FilePath
import qualified Filesystem.Path.CurrentOS as FS
import qualified Data.List as L

import           Development.Shake.Cpp.Paths

type Def = String
type Include = String
type LibPath = FilePath

data ObjType = Test | NonTest deriving Eq
data Target = 
  Src { path :: FilePath, objType :: ObjType } 
  -- ^ pre-cond, path is relative file name
  | Leaves { root :: FilePath, objType :: ObjType, isExec :: Bool } 
  -- ^ pre-cond, path is relative directory of object type 

-- | return all Cpp sources under sub path
sourceLeaves :: FilePath -> ObjType -> Bool -> BuildM Action [FilePath]
sourceLeaves sub_path obj_type use_exec = do 
  cpp_ext <- cppExtension . paths <$> build

  (parent, is_exec) <- 
    case obj_type of
      Test    -> do 
        pfx <- testPfx . paths <$> build 
        test_dir <- inputDir testObj
        return (test_dir, L.isPrefixOf pfx . basename) 
      NonTest -> do 
        main <- mainName . paths <$> build 
        src_dir <- inputDir sourceObj
        return (src_dir, (==) main . basename) 

  lift $
    L.filter ((||) use_exec . not . is_exec) <$> 
      getDirectoryFiles "" [(parent </> sub_path) ++ "//*." ++ cpp_ext]
  where
    basename :: FilePath -> String 
    basename = FS.encodeString . FS.basename . FS.filename . FS.decodeString

toFiles :: Target -> BuildM Action [FilePath]
toFiles (Src p ty) = 
  let file = FS.encodeString . FS.filename . FS.decodeString $ p in do
    paths' <- paths <$> build
    (bin_parent, filename) <- 
      case ty of 
        Test    -> (, file) <$> outputDir testObj
        NonTest -> (, testPfx paths' ++ file) <$> outputDir sourceObj
    return
      [bin_parent  
        </> (FS.encodeString . FS.directory . FS.decodeString $ p)
        </> (filename <.> objExtension paths')] 
toFiles (Leaves r ty e) = do 
  bin_parent <- outputDir (if ty == Test then testObj else sourceObj)
  obj_ext <- objExtension . paths <$> build
  map (\p -> (-<.> obj_ext) $ 
    bin_parent </> dropDirectory1 p) <$> sourceLeaves r ty e

data LibDeps = LibDeps { 
  includes :: [FilePath], linked :: [(Maybe String, [String])]
  }
  
data ToolChain = ToolChain {
  objCompiler ::
    [Def] -> LibDeps -> Debug -> FilePath -> FilePath -> Action (),
  exeCompiler :: 
    [Def] 
    -> [FilePath] -- ^ Object composition
    -> [FilePath] -- ^ Include paths 
    -> [(Maybe FilePath, [String])] -- ^ Library path and library
    -> Debug
    -> FilePath -- ^ The actual binary/exe output
    -> Action (), 
  objArchiver :: FilePath -> [FilePath] -> Action () 
}

type Debug = Bool

-- | The build environment, storing toolchain information, mode,
-- and path conventions.
data Env = Env { 
  debug :: Debug,
  paths :: Paths,
  toolchain :: ToolChain
  }

outputDir :: (Monad m, Functor m) => (BuildPaths -> Pair) -> BuildM m FilePath 
outputDir subpath = do 
  build_paths <- buildPaths . paths <$> build
  return $ outputPfx build_paths </> output (subpath build_paths) 

inputDir :: (Monad m, Functor m) => (BuildPaths -> Pair) -> BuildM m FilePath 
inputDir subpath = do 
  build_paths <- buildPaths . paths <$> build
  return $ input (subpath build_paths) 

input' :: (BuildPaths -> Pair) -> BuildPaths -> FilePath
input' f = input . f
   
type BuildM m a = ReaderT Env m a 

build :: (Monad m) => BuildM m Env 
build = ask 
      
runEnv ::  BuildM Rules () -> Env -> IO ()
runEnv build' = 
  shakeArgs (shakeOptions { shakeVerbosity = Loud }) . runReaderT build' 
