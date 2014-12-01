module Development.Shake.Cpp.ExecDeps where
import Data.Monoid
import Development.Shake.Cpp.Obj
import qualified Data.Set as S
import qualified Data.Map as M

-- data Linked = Linked {
--   linkedPaths :: S.Set FilePath, -- ^ e.g. ["/usr/lib/llvm-3.5/lib"]
--   libs :: S.Set String, -- ^ e.g. [boost_program_options, thread]
--   interpreted :: String
-- } deriving Show
  
data LinkerExp =
  LibPath FilePath 
  | Lib String
  | Evaluated String
  deriving (Show, Eq)
     
-- fromLists :: [FilePath] -> [String] -> Linked
-- fromLists paths libs' = 
--   Linked (S.fromList paths) (S.fromList libs') mempty 

-- fromLib :: FilePath -> [FilePath] -> Linked 
-- fromLib path = fromLists [path]  

-- instance Monoid Linked where
--   mempty      =
--     Linked mempty mempty mempty
--   mappend l r = 
--     Linked 
--       (mappend (linkedPaths l) (linkedPaths r)) 
--       (mappend (libs l) (libs r)) 
--       (mappend (interpreted l) (interpreted r)) 

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
