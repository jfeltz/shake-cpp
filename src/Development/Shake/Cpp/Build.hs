-- TODO eliminate cross-cutting of BuildM with InstallM 
{-# LANGUAGE TupleSections #-}
module Development.Shake.Cpp.Build where
import           Control.Applicative ((<$>))
import           Control.Monad.Reader
import           Development.Shake
import qualified Development.Shake.Iso as I
import           Development.Shake.Cpp.ExecDeps
import           Development.Shake.FilePath
-- import           Development.Shake.Cpp.Obj
import qualified Filesystem.Path.CurrentOS as FS
import qualified Data.List as L

import           Development.Shake.Cpp.Paths

type Def = String
type Include = String
type LibPath = FilePath

data Target = 
  Src { path :: FilePath, test :: Bool } 
  -- ^ pre-cond, path is relative file name
  | Leaves { root :: FilePath, test :: Bool, isExec :: Bool } 
  -- ^ pre-cond, path is relative directory of object type 

-- | return all Cpp sources under sub path
sourceLeaves :: FilePath -> Bool -> Bool -> BuildM Action [FilePath]
sourceLeaves sub_path test' use_exec = do 
  cpp_ext <- cppExtension . paths <$> build
  (parent, is_exec) <- 
    if test'
      then do 
        sfx <- testSfx . paths <$> build 
        test_dir <- inputDir testObj
        return (test_dir, L.isSuffixOf sfx . basename) 
      else do 
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
toFiles (Src p test') = 
  let file = FS.encodeString . FS.filename . FS.decodeString $ p in do
    paths' <- paths <$> build
    (bin_parent, filename) <- 
      if test' 
        then (, file) <$> outputDir testObj
        else (, file ++ testSfx paths') <$> outputDir sourceObj
    return
      [bin_parent  
        </> (FS.encodeString . FS.directory . FS.decodeString $ p)
        </> (filename <.> objExtension paths')] 
toFiles (Leaves r test' e) = do 
  bin_parent <- outputDir (if test' then testObj else sourceObj)
  obj_ext <- objExtension . paths <$> build
  map (\p -> (-<.> obj_ext) $ bin_parent </> dropDirectory1 p) 
    <$> sourceLeaves r test' e

data ToolChain = ToolChain {
  objCompiler ::
    [Def] 
    -> [FilePath] -- ^ Include paths
    -> Debug 
    -> FilePath -- ^ src file
    -> FilePath -- ^ dst obj file
    -> Action (),
  exeCompiler :: 
    [FilePath] -- ^ Object composition
    -> [LinkerExp] -- ^ Linked and library paths 
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

outputDir :: (Monad m, Functor m) => (BuildPaths -> I.Iso) -> BuildM m FilePath 
outputDir subpath = do 
  bp <- buildPaths . paths <$> build
  return $ I.outputPath (subpath bp) (outputPfx bp)

-- Convienence function, as calling Iso's instead gets messy quick.
inputDir :: (Monad m, Functor m) => (BuildPaths -> I.Iso) -> BuildM m FilePath 
inputDir subpath = I.input . subpath . buildPaths . paths <$> build

type BuildM m a = ReaderT Env m a 

build :: (Monad m) => BuildM m Env 
build = ask 
