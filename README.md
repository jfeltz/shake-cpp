A Shake C++ Framework 
---------------------------
This project is a practical example of using shake against a fairly small C++
build problem case. The design concepts of *shake-cpp* can likely be applied to 
non-C++ build problems as well, so this is being made public for
educational purposes to the Shake/Haskell community.

The following is an experimental abstraction layer for the [Shake build system](https://hackage.haskell.org/package/shake) , specifically for abstracting away concerns of:
  
  * C++ toolchain, e.g. linker and compiler
  * testing tools
  * path conventions, e.g. ``./src`` vs ``./sources`` 

Main design approach:
    
  * **shake-cpp** moves ```*.cpp``` **<->** ```*.o``` and ```*.obj``` **<->** ```*.exe``` etc isomorphisms to a single data-structure, and generalizes operations on that. This approach works very well in managing the complexity of shake rule patterns:

```haskell
buildPaths :: FilePath -> BuildPaths 
buildPaths build_par =  
  BuildPaths {  
    outputPfx   = build_par, -- E.g. .build/ or build_ or dist etc
    testLib     = "test-lib",
    -- An isomorphism, the second member is prefixed with outputPfx,
    -- so ".build/bin/a/b/last.o" when going from left to right,
    --   for input "src/a/b/last.cc"
    sourceObj   = Iso "src"   "bin", 
    testObj     = Iso "tests" "tests", 
    testExec    = Iso (build_par </> "tests") "test-bin",
    testStates  = Iso (build_par </> "test-bin") "test-state",
    archives    = "lib"
   }
```

  * **shake-cpp** enhances shake rules with a monad, ```BuildM``` that passes an environment ``Env`` to build targets and tool-chain calls.

```haskell
srcRules :: BuildM Rules ()
srcRules = do 
  -- rule for an archive, named "./lib/project-lib.o", comprised of sources
  -- under the src/foo/ directory
  Rule.archive "project-lib" [Leaves "foo" False False]

  -- rule for objects built from sources in "src/"
  Rule.object sourceObj [] Deps.llvmObj 

testRules :: BuildM Rules ()
testRules = do 
  src_root <- inputDir sourceObj
  test_root <- inputDir testObj

  Rule.object testObj []
    (Deps.llvmObj <> Deps.boostTestObj src_root test_root) 

  Rule.testExecs 
    (Deps.boostTestExec projectName <> Deps.clang <> Deps.llvmExec ) 

  -- Rule for test pass states, this makes "./shake .build/test-state/test_example.pass" 
  -- as a test runner case possible
  Rule.test_states 
  
  -- Bind source rules into this monad (this is a super-set of those)
  srcRules
  
  -- clean rule, derived from BuildPaths
  Rule.clean
```

and in module **Deps**:
```haskell
module Deps where

import Development.Shake.Cpp.Build (Def)
import Development.Shake.Cpp.ObjectDeps
import Development.Shake.Cpp.ExecDeps
import Development.Shake.Cpp.Obj
import Data.Monoid
import qualified Data.Set as S 
import qualified Data.Map as M
import qualified Data.List as L

boostTestExec :: FilePath -> ExecDeps
boostTestExec subject_archive = 
  ExecDeps 
    (M.singleton subject_archive Archive)
    $ L.map Lib
        ["boost_system", "boost_thread", "boost_unit_test_framework"]

boostTestObj :: FilePath -> FilePath -> ObjectDeps     
boostTestObj src_root test_root = 
  ObjectDeps 
    (S.fromList ["BOOST_TEST_MAIN","BOOST_TEST_DYN_LINK"])
    (S.fromList [src_root, test_root])

llvmInc :: FilePath
llvmInc = "/usr/lib/llvm-3.5/include"

llvmDefs :: S.Set Def 
llvmDefs = 
  S.fromList 
    ["NDEBUG",
     "_GNU_SOURCE",
     "__STDC_CONSTANT_MACROS",
     "__STDC_FORMAT_MACROS",
     "__STDC_LIMIT_MACROS"]

llvmObj :: ObjectDeps
llvmObj = 
  ObjectDeps llvmDefs (S.fromList ["/usr/lib/llvm-3.5/include"]) 

llvmExec :: ExecDeps 
llvmExec = ExecDeps mempty [Evaluated "llvm-config --libs all --ldflags"] 

clang :: ExecDeps
clang = mempty { 
  exeLinked = 
    L.map Lib
      ["pthread",
       "tinfo", 
      "dl", 
      "clangTooling", 
      "clangFrontendTool", 
      "clangFrontend", 
      "clangDriver", 
      "clangSerialization", 
      "clangCodeGen", 
      "clangParse", 
      "clangSema", 
      "clangStaticAnalyzerFrontend", 
      "clangStaticAnalyzerCheckers", 
      "clangStaticAnalyzerCore", 
      "clangAnalysis", 
      "clangARCMigrate", 
      "clangRewriteFrontend", 
      "clangEdit", 
      "clangAST", 
      "clangASTMatchers", 
      "clangLex", 
      "clangBasic", 
      "z"]
    }
```
