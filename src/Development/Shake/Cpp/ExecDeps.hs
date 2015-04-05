module Development.Shake.Cpp.ExecDeps where
import Data.Monoid
import Development.Shake.Cpp.Obj
import qualified Data.Map as M
import qualified Data.List as L

data LinkerExp =
  LibPath FilePath 
  | Lib String
  | Evaluated String
  deriving (Show, Eq)
     
data ExecDeps = ExecDeps {
  builtDeps :: M.Map FilePath Obj, 
  exeLinked :: [LinkerExp]
  }

instance Monoid ExecDeps where
  mempty      = ExecDeps mempty mempty
  mappend l r = 
    ExecDeps 
      (mappend (builtDeps l) (builtDeps r)) 
      (mappend (exeLinked l) (exeLinked r)) 

fromLibs :: [String] -> ExecDeps
fromLibs = ExecDeps mempty . L.map Lib 
