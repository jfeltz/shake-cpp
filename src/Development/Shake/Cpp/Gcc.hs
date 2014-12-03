module Development.Shake.Cpp.Gcc where
import qualified Data.List as L
import           Development.Shake hiding (Env)
import           Development.Shake.Cpp.Build
import           Development.Shake.FilePath
import           Development.Shake.Util
import           Development.Shake.Cpp.ExecDeps
import           Development.Shake.Cpp.Paths
import           Development.Shake.Cpp.Ld

gccFlag :: Char -> String -> String 
gccFlag _    []  = [] 
gccFlag flag arg = '-':(flag:arg)

gccIncludes :: [Include] -> String
gccIncludes = unwords . L.map (gccFlag 'I')

gccDefs :: [Def] -> String
gccDefs = unwords . L.map (gccFlag 'D')

gccExpressions :: [LinkerExp] -> Action String
gccExpressions [] = 
   return [] 
gccExpressions (LibPath p:rest)        = do 
   rest' <- gccExpressions rest 
   return $ gccFlag 'L' p ++ ' ':rest' 
gccExpressions (Lib name:rest)         = do 
   rest' <- gccExpressions rest 
   return $ gccFlag 'l' name ++ ' ':rest' 
gccExpressions (Evaluated expr:rest) = do 
   Stdout flags <- cmd expr 
   rest' <- gccExpressions rest 
   return $ flags ++ ' ': ' ':rest' 

objCompiler ::  [Def] -> [String] -> Debug -> FilePath -> FilePath -> Action ()
objCompiler defs includes debug' src dst = 
  let m = dst -<.> "m" 
      debug_opts = if debug' then "-O0" else []
  in do 
    () <- cmd
        ("g++ " ++ debug_opts ++ " -std=c++11 -c")
        src
        (" -o " ++ dst)
        (gccIncludes includes)
        (gccDefs defs)

        -- generate m, which stores dependency information for the obj 
        "-MMD -MF"
        [m]

    -- Bind (.o -> cc) dependency action on difference of 'm' 
    -- Thus, on run-cases of this monad other than the first, 
    -- the above cmd is executed if not met by dependency 
    needMakefileDependencies m

exeCompiler :: 
  [FilePath] 
  -> [LinkerExp]
  -> Debug
  -> FilePath 
  -> Action ()
exeCompiler objects linked debug' exec_bin = 
  let debug_opts = if debug' then "-O0" else [] in do
    link_expressions <- gccExpressions linked
    cmd 
      ("g++ " ++ debug_opts ++ " -std=c++11 -o")
      exec_bin
      objects
      link_expressions
  -- where
  --  fromInterpreted :: String -> Action String 
  --  fromInterpreted []   = return [] 
  --  fromInterpreted expr = do
  --    Stdout out <- cmd expr 
  --    return out

defaultEnv :: Debug -> Env
defaultEnv debug' = 
  Env { 
    debug     = debug',
    paths     = defaultPaths ".build",
    toolchain = ToolChain { 
      Development.Shake.Cpp.Build.objCompiler =
       Development.Shake.Cpp.Gcc.objCompiler,
      objArchiver = archive,
      Development.Shake.Cpp.Build.exeCompiler = 
       Development.Shake.Cpp.Gcc.exeCompiler
    }
  }
