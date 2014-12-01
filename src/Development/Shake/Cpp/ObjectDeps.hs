module Development.Shake.Cpp.ObjectDeps where
import qualified Data.Set as S
import           Data.Monoid
-- import qualified Data.Trie as T
-- import qualified Data.List as L
-- import qualified Data.Map as M
-- import Data.ByteString.Char8 (pack) 
-- import System.FilePath.Posix

data ObjectDeps = ObjectDeps { 
   defs :: S.Set String, 
   includes :: S.Set FilePath
  } deriving Show
  
instance Monoid ObjectDeps where
  mempty       = ObjectDeps mempty mempty
  mappend l r  = 
    ObjectDeps {
      defs = mappend (defs l) (defs r)
      , includes = mappend (includes l) (includes r) 
      }
  
-- Parts of a test or source object tree must be annotated
-- with dependencies

-- pathDependencies :: [(FilePath, ObjectDeps)] -> T.Trie ObjectDeps
-- pathDependencies paths =  
     

-- | A Trie expressed in terms of paths instead of characters. 
-- This isn't efficient at all, and it is converted later
-- into its bytestring counterpart. 
-- data PathTrie = 
--   Map { mapping :: M.Map FilePath (ObjectDeps, PathTrie) } | Leaf ObjectDeps

-- fromPath :: [FilePath] -> ObjectDeps -> PathTrie 
-- fromPath [] object_deps       = 
--   Leaf object_deps 
-- fromPath (p:rest) object_deps = 
--   Map . M.singleton p $ (mempty, fromPath rest object_deps) 

-- pathError :: [FilePath] -> r
-- pathError paths = 
--   error $ "erroneous build defined on path: " ++ joinPath paths

-- -- | Associate a path with an object dependency
-- assoc :: [FilePath] -> ObjectDeps -> PathTrie -> PathTrie
-- assoc [] object_dep  _                   =
--   Leaf object_dep
-- assoc (subpath:rest) _  (Leaf _)         =
--   pathError (subpath:rest) 
-- assoc (subpath:rest) object_deps (Map m) = 
--   Map $ M.alter (Just . f) subpath m where 
--     f :: Maybe (ObjectDeps, PathTrie) -> (ObjectDeps, PathTrie) 
--     -- subpath isn't in the map 
--     f Nothing            = (mempty, fromPath rest object_deps) 
--     f (Just (_, Leaf _)) = pathError (subpath:rest) 
--     f (Just (deps, t))   = (deps, assoc rest object_deps t)

-- -- | Convert path trie to an optimal ByteString trie,
-- -- and hold a ObjectDep inheritance property
-- bsTrie :: FilePath -> ObjectDeps -> PathTrie  -> T.Trie ObjectDeps
-- bsTrie path inherited path_trie = 
--   case path_trie of  
--     (Leaf deps) ->
--       T.singleton (pack path) $ inherited <> deps 
--     (Map mapping') ->
--       -- For each subpath (FilePath, ..) 
--       -- Each member of this list is disjoint, so unionL is safe
--       L.foldl T.unionL T.empty (map (uncurry toMember) $ M.toList mapping')
--     where
--       toMember :: FilePath -> (ObjectDeps, PathTrie) -> T.Trie ObjectDeps 
--       toMember sub_path (deps, t) = 
--         bsTrie (path </> sub_path) (inherited <> deps) t 
