-- | TODO test states will overlap with non-Cpp languages 

module Development.Shake.Cpp.Rule where
import Prelude hiding ((*>))
import           Development.Shake
import           Development.Shake.FilePath
import qualified Development.Shake.Cpp.Build as B
import qualified Development.Shake.Cpp.Paths as P
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
object :: (P.BuildPaths -> I.Iso) -> FilePath -> ObjectDeps -> B.BuildM Rules ()
object pf sub_path object_deps = do 
  obj_ext <- P.objExtension . B.paths <$> B.env
  obj_compiler <- B.objCompiler . B.toolchain <$> B.env
  bp <- B.buildPaths
  cpp_ext <- P.cppExtension . B.paths <$> B.env 
  debug' <- B.debug <$> B.env 
  bin <- B.outputDir pf
  
  -- FIXME may not be arch neutral
  let parent = normalise $ bin </> sub_path 

  lift $ (*>) (parent ++ "//*." ++ obj_ext) $ 
    \obj ->
      obj_compiler 
        (S.toList . defs $ object_deps)
        (S.toList . includes $ object_deps)
        debug' 
        (I.morphLeft cpp_ext (P.outputPfx bp) (pf bp) obj)
        obj
            
exec :: 
  String 
  -> Obj
  -> FilePath
  -> ExecDeps 
  -> B.BuildM Rules ()
exec name obj_type sub_path exec_deps = do 
  e <- B.env 
  bp <- B.buildPaths 
  main_obj <- mainObjPath 
  lift $ 
    (*>) 
      (toExeBin obj_type bp </> name <.> exe)
      (execCompile main_obj exec_deps e) 
  where
    mainObjPath = do 
      e <- B.env
      bp <- B.buildPaths
      paths' <- B.paths <$> B.env 
      return $ 
        (toLibBin 
          obj_type 
          bp </> sub_path </> (P.mainName . B.paths $ e)) 
          <.> P.objExtension paths'

execCompile :: FilePath -> ExecDeps -> B.Env -> FilePath -> Action ()
execCompile main_obj exe_deps b_env =
  let cf = B.exeCompiler $ B.toolchain b_env in
    \exec_bin -> do
     need $ main_obj : objects 
     cf (main_obj : objects) (exeLinked exe_deps) (B.debug b_env) exec_bin
  where 
    objects =
      map (uncurry (toFile (B.paths b_env))) (M.toList $ builtDeps exe_deps)

archive :: FilePath -> [B.Target] -> B.BuildM Rules () 
archive dst_obj target_deps = do 
  archiver_f  <- B.objArchiver . B.toolchain <$> B.env
  archive_bin <- P.archivePath <$> B.buildPaths
  obj_ext <- P.objExtension . B.paths <$> B.env
  e <- B.env

  lift $ (*>) (archive_bin </> dst_obj <.> obj_ext) $ \obj -> do 
    objects <- L.concat <$> mapM (\t -> runReaderT (B.toFiles t) e) target_deps
    need objects
    archiver_f obj objects
     
-- | Pre-conditiation: filepath is relative  
toFile :: P.Paths -> FilePath -> Obj  -> FilePath
toFile paths' rel dep = 
  (-<.> P.objExtension paths') (toLibBin dep (P.buildPaths paths') </> rel)

-- | A rule covering a general case for test executables. 
--   By convention, each test executable depends on an object following
--   test-bin/../test_pfx*.o 

testExecs :: ExecDeps -> B.BuildM Rules ()
testExecs exec_deps = do
  debug' <- B.debug <$> B.env
  paths' <- B.paths <$> B.env
  bp <- B.buildPaths
  cf <- B.exeCompiler . B.toolchain <$> B.env 
  test_exec_bin <- B.outputDir P.testExec  
  test_sfx <- P.testSfx . B.paths <$> B.env
  lift $
    -- Test rule dependent on objects source tree for 
    (*>) (test_exec_bin ++ "//" ++ "*" ++ test_sfx <.> exe) $ \exec' -> do
      let objects  = 
            map (uncurry (toFile paths'))  (M.toList $ builtDeps exec_deps)
          -- .build/tests/tests/ide/foo_test.o
          output_pfx = P.outputPfx bp
          test_obj   = 
            I.morphLeft (P.objExtension paths') output_pfx (P.testExec bp) exec'
      need $ test_obj : objects 
      cf (test_obj : objects) (exeLinked exec_deps) debug' exec'

-- Actions that satisfy a test pass state file 
-- The goal of this is as follows:
-- 1. run previously failed or changed test execs
-- 2. run new tests 
test_states :: B.BuildM Rules ()
test_states = do 
  bp <- B.buildPaths
  test_state_path <- B.outputDir P.testStates
  test_state_ext <- P.tsExtension . B.paths <$> B.env 
  
  lift $
    (*>) (test_state_path ++ "//*." ++ test_state_ext) $
      \state -> quietly $ do 
        -- Convert test state to test exec
        let test_exec = I.morphLeft exe (P.outputPfx bp) (P.testStates bp) state
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

clean :: B.BuildM Rules ()
clean = do 
  dir <- P.outputPfx <$> B.buildPaths  
  lift . phony "clean" $ removeFilesAfter dir ["//*"] 

-- | rule that when activated is only satisfied with all
-- test states passing
-- Note: this assumes Rule.test_states is bound 
all_test_states :: B.BuildM Rules ()
all_test_states = do
  paths' <- B.paths <$> B.env 
  test_dir    <- B.inputDir P.testObj
  test_states'<- B.outputDir P.testStates
  
  let cpp_extension  = P.cppExtension paths' 
      test_state_ext = P.tsExtension paths' 
      test_sfx    = P.testSfx paths' 

  lift . action $ do 
    testSources <- 
      getDirectoryFiles "" [test_dir ++ "//*"++ test_sfx ++ "." ++ cpp_extension]

    let relSources = map dropDirectory1 testSources
    need [test_states' </> p -<.> test_state_ext | p <- relSources]

-- | Convenience rules covering correlating test objects, execs, and states
testRules :: ObjectDeps -> ExecDeps -> B.BuildM Rules ()
testRules test_obj_deps test_exec_deps = do 
  object P.testObj [] test_obj_deps
  testExecs test_exec_deps 
  test_states 
