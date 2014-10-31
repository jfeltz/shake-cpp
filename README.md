A Shake C++ Framework 
---------------------------
This project is a practical example of using shake against a fairly small C++
build problem case. The design concepts of *shake-cpp* can likely be applied to
many different build problems as well, so this is being made public for
educational purposes to the Shake/Haskell community.

The following is an experimental abstraction layer for the [Shake build system](https://hackage.haskell.org/package/shake) , specifically for abstracting away concerns of:
  
  * C++ toolchain, e.g. linker and compiler
  * testing tools
  * path conventions, e.g. ``./src`` vs ``./sources`` 

Main design approach:
    
  * **shake-cpp** moves ```*.cpp``` **<->** ```*.o``` and ```*.obj **<->** ```*.exe``` isomorphisms to a single data-structure, and generalizes operations on that. This approach works very well in managing the complexity of shake rule patterns:

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
srcRules :: Maybe FilePath -> BuildM Rules ()
srcRules boost_root = do 
  src_root <- inputDir sourceObj
  
  -- rule for an archive, named "./lib/project-lib.o", comprised of sources
  -- under the src/foo/ directory
  Rule.archive "project-lib" [Leaves "foo" NonTest False]
  
  -- rule for objects built from sources in "src/"
  Rule.object sourceObj "" [] $
    LibDeps [src_root] [((</> "stage/lib") <$> boost_root, boostLibs)]

testRules :: Maybe FilePath -> BuildM Rules ()
testRules boost_root = do 
  src_root <- inputDir sourceObj
  test_root <- inputDir testObj
  
  -- rule defining test objects, using boost unit in this case
  Rule.object testObj [] 
    ["BOOST_TEST_MAIN","BOOST_TEST_DYN_LINK"] $
    LibDeps [src_root, test_root] [((</> "stage/lib") <$> boost_root, boostLibs)]
  
  -- rule defining build for test executables
  Rule.test_execs
    []
    Rule.ExecDeps {
      Rule.includeDeps = [],
      Rule.builtDeps   = [(Rule.Archive, "project-lib")], -- using "./lib/project-lib.o"
      Rule.exeLinked   = [((</> "stage/lib") <$> boost_root, boostTestLibs)]
    }
  
  -- Rule for test pass states, this makes "./shake .build/test-state/test_example.pass" 
  -- as a test runner case possible
  Rule.test_states 
  
  -- Bind source rules into this monad (this is a super-set of those)
  srcRules boost_root
  
  -- clean rule, derived from BuildPaths
  Rule.clean
```
