-- TODO eliminate cross-cutting of BuildM with InstallM 
{-# LANGUAGE TupleSections #-}
module Development.Shake.Cpp.Build where
import           Control.Applicative ((<$>))
import           Control.Monad.Reader
import           Development.Shake
import qualified Development.Shake.Iso as I
import           Development.Shake.Cpp.ExecDeps
import           Development.Shake.Cpp.ObjectDeps
import           Development.Shake.FilePath
import qualified Filesystem.Path.CurrentOS as FS
import qualified Data.List as L
import qualified Development.Shake.Cpp.Paths as P
import Data.Monoid
import qualified Data.Set as S

type Def = String
type Include = String
type LibPath = FilePath

type Debug = Bool

-- | The build environment, storing toolchain information, mode,
-- and path conventions.
data Env = Env { 
  debug :: Debug,
  paths :: P.Paths,
  toolchain :: ToolChain
  }

type BuildM m a = ReaderT Env m a 

{- various helpers for reaching into BuildM -}

env :: (Monad m) => BuildM m Env 
env = ask 

buildPaths :: (Monad m, Functor m) => BuildM m P.BuildPaths
buildPaths = P.buildPaths . paths <$> env

outputDir :: (Monad m, Functor m) => (P.BuildPaths -> I.Iso) -> BuildM m FilePath 
outputDir f = do 
  bp <- buildPaths
  return $ I.outputPath (f bp) (P.outputPfx bp)

inputDir :: (Monad m, Functor m) => (P.BuildPaths -> I.Iso) -> BuildM m FilePath 
inputDir f = I.input . f <$> buildPaths

srcObjDeps :: (Monad m, Functor m) => BuildM m ObjectDeps 
srcObjDeps = ObjectDeps mempty . S.singleton <$> inputDir P.sourceObj

testObjDeps :: (Monad m, Functor m) => BuildM m ObjectDeps 
testObjDeps = ObjectDeps mempty . S.singleton <$> inputDir P.testObj

data Target = 
  Src { path :: FilePath, test :: Bool } 
  -- ^ pre-cond, path is relative file name
  | Leaves { root :: FilePath, test :: Bool, isExec :: Bool } 
  -- ^ pre-cond, path is relative directory of object type 

-- | return all Cpp sources under sub path
sourceLeaves :: FilePath -> Bool -> Bool -> BuildM Action [FilePath]
sourceLeaves sub_path test' use_exec = do 
  cpp_ext <- P.cppExtension . paths <$> env
  (parent, is_exec) <- 
    if test'
      then do 
        sfx <- P.testSfx . paths <$> env 
        test_dir <- inputDir P.testObj
        return (test_dir, L.isSuffixOf sfx . basename) 
      else do 
        main <- P.mainName . paths <$> env 
        src_dir <- inputDir P.sourceObj
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
    paths' <- paths <$> env
    (bin_parent, filename) <- 
      if test' 
        then (, file) <$> outputDir P.testObj
        else (, file ++ P.testSfx paths') <$> outputDir P.sourceObj
    return
      [bin_parent  
        </> (FS.encodeString . FS.directory . FS.decodeString $ p)
        </> (filename <.> P.objExtension paths')] 
toFiles (Leaves r test' e) = do 
  bin_parent <- outputDir (if test' then P.testObj else P.sourceObj)
  obj_ext <- P.objExtension . paths <$> env
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
