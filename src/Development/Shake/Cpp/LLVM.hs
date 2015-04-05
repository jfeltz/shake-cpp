module Development.Shake.Cpp.LLVM where

import Development.Shake.Cpp.Build (Def)
import Development.Shake.Cpp.ExecDeps 
import Development.Shake.Cpp.ObjectDeps 
import qualified Data.List as L
import qualified Data.Set as S
import Data.Monoid

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
clang = 
  fromLibs 
    ["pthread",
     "tinfo", 
    "dl", 
    "clang",
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
