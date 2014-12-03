-- | TODO additional methods that read from a pattern -> BuildM Rules () 
-- mapping for custom cases, since not all tests and sources will link the same

module Development.Shake.Cpp.Rule where
import           Development.Shake
import           Development.Shake.FilePath
import           Development.Shake.Cpp.Build
import           Development.Shake.Cpp.Paths
import           Development.Shake.Cpp.ExecDeps
import           Development.Shake.Cpp.ObjectDeps
import           Development.Shake.Cpp.Obj
import qualified Development.Shake.Iso as I

import           Control.Applicative ((<$>))
import           Control.Monad.Reader
import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.Map as M
import           System.Exit

-- | Rule for objects rooted at sub-path
object :: (BuildPaths -> I.Iso) -> FilePath -> ObjectDeps -> BuildM Rules ()
object pf sub_path object_deps = do 
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
        (S.toList . defs $ object_deps)
        (S.toList . includes $ object_deps)
        debug' 
        (I.morphLeft cpp_ext (outputPfx bp) (pf bp) obj)
        obj
            
exec :: 
  String 
  -> Obj
  -> FilePath -- The parent in the source tree 
  -> ExecDeps 
  -> BuildM Rules ()-- Deps that can be anywhere
exec name obj_type sub_path exec_deps = do 
  b <- build
  main_obj <- mainObjPath 
  lift $ 
    (*>) 
      (toExeBin obj_type (buildPaths $ paths b) </> name <.> exe)
      (execCompile main_obj exec_deps b) 
  where
    mainObjPath = do 
      b <- build
      return $ 
        (toLibBin 
          obj_type 
          (buildPaths $ paths b) </> sub_path </> (mainName . paths $ b)) 
          <.> objExtension (paths b)

execCompile :: FilePath -> ExecDeps -> Env -> FilePath -> Action ()
execCompile main_obj exe_deps b =
  let cf = exeCompiler $ toolchain b in
    \exec_bin -> do
     need $ main_obj : objects 
     cf 
       (main_obj : objects)
       (exeLinked exe_deps)
       (debug b)
       exec_bin
  where 
    objects = map (uncurry (toFile (paths b))) (M.toList $ builtDeps exe_deps)

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
     
-- | Pre-conditiation: filepath is relative  
toFile :: Paths -> FilePath -> Obj  -> FilePath
toFile paths' rel dep = 
  (-<.> objExtension paths') (toLibBin dep (buildPaths paths') </> rel)

-- | Env a rule covering a general case for test executables. 
--   By convention, each test executable depends on an object following
--   test-bin/../test_pfx*.o 

testExecs :: ExecDeps -> BuildM Rules ()
testExecs exec_deps = do
  b <- build
  let cf = exeCompiler . toolchain $ b 
  --let test_exec_bin = outputPath testExec . buildPaths . paths $ b 
  test_exec_bin <- outputDir testExec  
  let test_sfx = testSfx . paths $ b
  lift $
    -- Test rule dependent on objects source tree for 
    (*>) (test_exec_bin ++ "//" ++ "*" ++ test_sfx <.> exe) $ \exec' -> do
      let objects  = 
            map (uncurry (toFile $ paths b))  (M.toList $ builtDeps exec_deps)
          -- .build/tests/tests/ide/foo_test.o
          output_pfx = 
            outputPfx . buildPaths . paths $ b
          test_obj = 
            I.morphLeft "o" output_pfx (testExec . buildPaths . paths $ b) exec'
      need $ test_obj : objects 
      cf
       (test_obj : objects)
       (exeLinked exec_deps)
       (debug b)
       exec'

-- Actions that satisfy a test pass state file 
-- The goal of this is as follows:
-- 1. run previously failed or changed test execs
-- 2. run new tests 
test_states :: BuildM Rules ()
test_states = do 
  test_state_path <- outputDir testStates
  test_state_ext  <- tsExtension . paths <$> build
  bp <- buildPaths . paths <$> build 
  
  lift $
    (*>) (test_state_path ++ "//*." ++ test_state_ext) $
      \state -> quietly $ do 
        -- Convert test state to test exec
        -- let test_exec = morphLeft bp [] testStates state 
        let test_exec = I.morphLeft exe (outputPfx bp) (testStates bp) state 
        need [test_exec]
        -- Note:
        --   This will fail to satisfy the rule if either command fails.

        -- Accessing the exit code minimizes output by shake to be reproduced,
        -- otherwise it's unnecessary to build this rule.

        -- However this also means that we now need to check the result,
        -- otherwise shake will not consider the ret code as a failing case. 
        (Exit c) <- command [WithStderr False] test_exec []
        let (ran, nostic) = run_case c 
        (liftIO . putStrLn $ nostic ++ " exit code received") >> cmd ran state
     where
        run_case (ExitFailure _) = ("rm", "non-zero") 
        run_case ExitSuccess     = ("touch", "zero") 

clean :: BuildM Rules ()
clean = do 
  dir <- outputPfx . buildPaths . paths <$> build 
  lift . phony "clean" $ removeFilesAfter dir ["//*"] 

-- | rule that when activated is only satisfied with all
-- test states passing
-- Note: this assumes Rule.test_states is bound 
all_test_states :: BuildM Rules ()
all_test_states = do
  test_dir       <- inputDir testObj
  test_sfx       <- testSfx . paths <$> build
  test_states'    <- outputDir testStates
  cpp_extension  <- cppExtension . paths <$> build
  test_state_ext <- tsExtension . paths <$> build

  lift . action $ do 
    testSources <- 
      getDirectoryFiles "" [test_dir ++ "//*"++ test_sfx ++ "." ++ cpp_extension]

    let relSources = map dropDirectory1 testSources
    liftIO . print $ relSources
    need [test_states' </> p -<.> test_state_ext | p <- relSources]

testRules :: ObjectDeps -> ExecDeps -> BuildM Rules ()
testRules test_obj_deps test_exec_deps = do 
  object testObj [] test_obj_deps
  testExecs test_exec_deps 
  -- rule defining build for test executables
  -- based on build configuration, the prefix and test directory
  -- are already defined, however the dependencies may cross 
  -- between objects built in the test directory and elsewhere
  
  -- Rule for test pass states
  test_states 
