module Development.Shake.Cpp.Boost where
import Development.Shake.Cpp.ExecDeps 
import Development.Shake.Cpp.ObjectDeps 
import Development.Shake.Cpp.Obj
import qualified Development.Shake.Cpp.Build as B
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.List as L
import Control.Monad 
import Data.Monoid 
import Control.Applicative 

testExecDeps :: String -> ExecDeps
testExecDeps subject_archive = 
  ExecDeps 
    (M.singleton subject_archive Archive)
    $ L.map Lib
        ["boost_system", "boost_thread", "boost_unit_test_framework"]

testObjDeps :: (Monad m, Functor m) => B.BuildM m ObjectDeps     
testObjDeps = 
  (<>) (fromDefs ["BOOST_TEST_MAIN","BOOST_TEST_DYN_LINK"]) <$> 
    liftM2 (<>) B.testObjDeps B.srcObjDeps
