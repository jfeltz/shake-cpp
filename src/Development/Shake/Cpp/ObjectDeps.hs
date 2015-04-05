module Development.Shake.Cpp.ObjectDeps where
import qualified Data.Set as S
import           Data.Monoid

data ObjectDeps = ObjectDeps { 
   defs :: S.Set String, 
   includes :: S.Set FilePath
  } deriving Show
  
instance Monoid ObjectDeps where
  mempty       = 
    ObjectDeps mempty mempty
  mappend l r  = 
    ObjectDeps {
      defs = mappend (defs l) (defs r)
      , includes = mappend (includes l) (includes r) 
      }

fromDefs :: [String] -> ObjectDeps
fromDefs defs' = mempty { defs = S.fromList defs' } 

fromIncludes :: [String] -> ObjectDeps
fromIncludes includes' = mempty { includes = S.fromList includes' } 
