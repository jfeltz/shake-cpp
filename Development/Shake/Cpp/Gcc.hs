module Development.Shake.Cpp.Gcc where
import qualified Data.List as L
import           Development.Shake
import           Development.Shake.Cpp.Build
import           Development.Shake.FilePath
import           Development.Shake.Util

gccFlag :: Char -> String -> String 
gccFlag _    []  = [] 
gccFlag flag arg = '-':(flag:arg)

gccIncludes :: [Include] -> String
gccIncludes = unwords . L.map (gccFlag 'I')

gccDefs :: [Def] -> String
gccDefs = unwords . L.map (gccFlag 'D')
  
gccLibs :: [(Maybe String, [String])] -> String 
gccLibs = unwords . L.map (uncurry libGroup) where
  libGroup :: Maybe String -> [String] -> String
  libGroup searchpath members = 
    maybe [] (gccFlag 'L') searchpath ++ unwords (L.map (gccFlag 'l') members)

-- TODO use debug flag, add header dependency calls
objCompiler ::  [Def] -> LibDeps -> Debug -> FilePath -> FilePath -> Action ()
objCompiler defs deps debug' src dst = do
     let m = dst -<.> "m" 
     () <- cmd
       "g++ -std=c++11 -c"
       src
       (" -o " ++ dst)
       (gccIncludes $ includes deps)
       (gccLibs $ linked deps)
       (gccDefs defs)
       "-MMD -MF" 
       [m]
     -- Assign (.o -> cc) dependency on difference of 'm' 
     -- Thus, on all calls to this monad other than the first, 
     -- cmd is executed if not met by makefile dependency 
     needMakefileDependencies m

-- TODO use debug flag
exeCompiler :: 
  [Def] 
  -> [FilePath] 
  -> [FilePath] 
  -> [(Maybe FilePath, [String])] 
  -> Debug
  -> FilePath 
  -> Action ()
exeCompiler defs objects includes' deps debug' exec_bin = 
  cmd "g++ -std=c++11 -o"
     exec_bin 
     objects
     (gccIncludes includes')
     (gccDefs defs)
     (gccLibs deps)
