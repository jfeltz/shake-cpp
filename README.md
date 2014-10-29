Shake C++ Abstraction Layer
---------------------------

shake-cpp
=========
The following is an experimental abstraction layer for the shake build system, specifically for abstracting away concerns of:
  
  * C++ toolchain, e.g. linker and compiler
  * testing tools
  * path conventions, e.g. ``./src`` vs ``./sources`` 


This project is being published mainly for the benefit of the Haskell Shake community. It is a practical example of using shake against a fairly small C++ build problem case, and so it should be fairly educational.

Main design features & approach:
    
  * separate the source <-> obj and obj <-> exec isomorphisms in a single data-structure, and generalize operations on that. This approach works very well in managing the complexity of shake rule patterns:
  * 
```haskell
buildPaths :: FilePath -> BuildPaths 
buildPaths build_par =  
  BuildPaths {  
    outputPfx   = build_par, 
    testLib     = "test-lib",
    sourceObj   = Pair "src"   "bin", 
    testObj     = Pair "tests" "tests", 
    testExec    = Pair (build_par </> "tests") "test-bin",
    testStates  = Pair (build_par </> "test-bin") "test-state",
    archives    = "lib"
   }
```

  * Instead of replacing shake rules, it enhances them with a Monad, ```BuildM``` that passes an environment ``Env`` to build targets and tool-chain members.


```haskell
srcRules :: Maybe FilePath -> BuildM Rules ()
srcRules boost_root = do 
  src_root <- inputDir sourceObj

  Rule.archive "project-lib" [Leaves "project" NonTest False]
  
  Rule.object sourceObj "" [] $
    LibDeps [src_root] [((</> "stage/lib") <$> boost_root, boostLibs)]

testRules :: Maybe FilePath -> BuildM Rules ()
testRules boost_root = do 
  src_root <- inputDir sourceObj
  test_root <- inputDir testObj

  Rule.object testObj [] 
    ["BOOST_TEST_MAIN","BOOST_TEST_DYN_LINK"] $
    LibDeps [src_root, test_root] [((</> "stage/lib") <$> boost_root, boostLibs)]
  
  -- rule defining build for test executables
  -- based on build configuration, the prefix and test directory
  -- are already defined, however the dependencies may cross 
  -- between objects built in the test directory and elsewhere
  
  Rule.test_execs
    []
    Rule.ExecDeps {
      Rule.includeDeps = [],
      Rule.builtDeps   = [(Rule.Archive, "project-lib")], 
      Rule.exeLinked   = [((</> "stage/lib") <$> boost_root, boostTestLibs)]
    }
  
  -- Rule for test pass states, TODO command line flags to pass to test
  Rule.test_states 
  
  -- Bind source rules into this monad (this is a super-set of those)
  srcRules boost_root
  
  Rule.clean
```
  

