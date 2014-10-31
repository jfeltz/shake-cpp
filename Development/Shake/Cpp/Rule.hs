-- | TODO additional methods that read from a pattern -> BuildM Rules () 
-- mapping for custom cases, since not all tests and sources will link the same

module Development.Shake.Cpp.Rule where
import           Development.Shake
import           Development.Shake.FilePath
import           Development.Shake.Cpp.Build
import           Development.Shake.Cpp.Paths

import           Control.Applicative ((<$>))
import           Control.Monad.Reader
import qualified Data.List as L
import           System.Exit

-- | Rule for objects rooted at sub-path
object :: (BuildPaths -> Iso) -> FilePath -> [Def] -> LibDeps -> BuildM Rules ()
object pf sub_path defs obj_deps = do 
  obj_ext <- objExtension . paths <$> build
  obj_compiler <- objCompiler . toolchain <$> build
  bp <- buildPaths . paths <$> build 
  cpp_ext <- cppExtension . paths <$> build 
  debug' <- debug <$> build 
  bin <- outputDir pf
  
  -- FIXME may not be arch neutral
  let parent = normalise $ bin </> sub_path 

  lift $ (*>) (parent ++ "//*." ++ obj_ext) $ 
    \obj ->
      obj_compiler 
        defs 
        obj_deps 
        debug' 
        (morphLeft bp cpp_ext pf obj)
        obj

-- | Return the object parent location for an object type 
toLibBin :: ObjDep -> BuildPaths -> FilePath 
toLibBin Archive = archivePath 
toLibBin Obj     = outputPath sourceObj
toLibBin TestObj = testLibPath 
            
exec :: 
  String ->
  ObjDep -> -- The object type that determines source location 
  FilePath -> -- The parent in the source tree 
  [Def] -> 
  ExecDeps -> -- Deps that can be anywhere 
  BuildM Rules ()
exec name obj_type sub_path defs exec_deps = do 
  b <- build
  main_obj <- mainObjPath 
  lift $ 
    (*>) 
      -- In this case the destination executable is not necessarily 
      -- test_ for a test object, this is a different scenario 
      (fromObjDep obj_type (buildPaths $ paths b) </> name <.> exe) -- the pattern for the exe
      (execCompile defs main_obj exec_deps b) 
  where
    mainObjPath = do 
      b <- build
      return $ 
        (toLibBin obj_type (buildPaths $ paths b) </> sub_path </> (mainName . paths $ b)) 
          <.> objExtension (paths b)
    -- | return a target directory for the exe
    -- from the object type
    fromObjDep :: ObjDep -> BuildPaths -> FilePath 
    fromObjDep Archive = archivePath
    fromObjDep Obj     = outputPath sourceObj
    fromObjDep TestObj = outputPath testObj

execCompile :: [Def] -> FilePath -> ExecDeps -> Env -> FilePath -> Action ()
execCompile defs main_obj exe_deps b =
  let cf = exeCompiler $ toolchain b in
    \exec_bin -> do
     let objects = map (uncurry (toFile (paths b)))  (builtDeps exe_deps)
     need $ main_obj : objects 
     cf 
       defs
       (main_obj : objects)
       (includeDeps exe_deps)
       (exeLinked exe_deps)
       (debug b)
       exec_bin

archive :: FilePath -> [Target] -> BuildM Rules () 
archive dst_obj target_deps = do 
  archiver_f  <- objArchiver . toolchain <$> build
  archive_bin <- archivePath . buildPaths . paths <$> build
  obj_ext <- objExtension . paths <$> build
  b <- build

  lift $ (*>) (archive_bin </> dst_obj <.> obj_ext) $ \obj -> do 
    objects <- L.concat <$> mapM (\t -> runReaderT (toFiles t) b) target_deps
    need objects
    archiver_f obj objects
     
data ObjDep = Archive | Obj | TestObj

-- | Pre-conditiation: filepath is relative  
toFile :: Paths -> ObjDep -> FilePath  -> FilePath
toFile paths' dep rel = 
  (-<.> objExtension paths') (toLibBin dep (buildPaths paths') </> rel)

-- TODO Exec should be scoped to a separate module, 
data ExecDeps = ExecDeps {
  includeDeps :: [FilePath],
  builtDeps :: [(ObjDep, FilePath)],
  exeLinked :: [(Maybe String, [String])]
  }
  
-- | Env a rule covering a general case for test executables. 
--   By convention, each test executable depends on an object following
--   test-bin/../test_pfx*.o 

test_execs :: [Def] -> ExecDeps -> BuildM Rules ()
test_execs defs exec_deps = do
  b <- build 
  let cf = exeCompiler . toolchain $ b 
  let test_exec_bin = outputPath testExec . buildPaths . paths $ b 
      test_pfx = testPfx . paths $ b
  -- error $ "p: " ++ test_exec_bin
  lift $ -- FIXME
    (*>) (test_exec_bin ++ "//" ++ test_pfx ++ "*" <.> exe) $ \exec' -> do
      let objects = map (uncurry (toFile (paths b)))  (builtDeps exec_deps)
          test_obj = morphLeft (buildPaths . paths $ b) "o" testExec exec' 
      need $ test_obj : objects 
      cf
       defs
       (test_obj : objects)
       (includeDeps exec_deps)
       (exeLinked exec_deps)
       (debug b)
       exec'

-- Actions that satisfy a test pass state file 
-- The goal of this is as follows:
-- 1. run previously failed or changed test execs
-- 2. run new tests 
test_states :: BuildM Rules ()
test_states = do 
  test_state_path <- outputPath testStates . buildPaths . paths <$> build  
  test_state_ext  <- tsExtension . paths <$> build
  bp <- buildPaths . paths <$> build 
  
  lift $
    (*>) (test_state_path ++ "//*." ++ test_state_ext) $
      \state -> quietly $ do 
        -- Convert test state to test exec
        let test = morphLeft bp [] testStates state 
        need [test]
        -- Note:
        --   This will fail to satisfy the rule if either command fails.

        -- Accessing the exit code minimizes output by shake to be reproduced,
        -- otherwise it's unnecessary to build this rule.

        -- However this also means that we now need to check the result,
        -- otherwise shake will not consider the ret code as a failing case. 
        (Exit c) <- command [WithStderr False] test []
        case c of 
          (ExitFailure _) -> return () 
          ExitSuccess     -> cmd "touch" state

clean :: BuildM Rules ()
clean = do 
  dir <- outputPfx . buildPaths . paths <$> build 
  lift . phony "clean" $ removeFilesAfter dir ["//*"] 
