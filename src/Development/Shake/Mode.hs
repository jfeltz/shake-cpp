module Development.Shake.Mode where
import Development.Shake
import Control.Monad.Reader.Rules
import qualified Data.Map as M
import qualified Data.List as L
import qualified System.Environment as E
import           Development.Shake.Cpp.Build
import qualified Development.Shake.Cpp.Rule as Rule
import           Development.Shake.Install

data Mode = Mode { title :: String, kw :: String, io :: IO () } 

data Modes = Modes { 
  def :: (String, IO ()),
  toMode :: M.Map String (String, IO ()) 
  }
  
insert :: Mode -> Modes -> Modes 
insert mode modes = 
  modes { toMode = M.insert (kw mode) (title mode, io mode) (toMode modes) } 

fromMode :: Mode -> Modes
fromMode test_mode = 
  insert test_mode $ Modes (kw test_mode, io test_mode) M.empty 
     
installMode :: Verbosity -> InstallM Rules () -> Install -> Mode
installMode verbosity rules install' = 
  Mode "Install" "install" $ runRules verbosity rules install' 

testMode :: BuildM Rules () -> (Debug -> Env) -> Mode
testMode test_rules to_env = 
  Mode "Test" "test" $ do
    test_goals <- E.getArgs

    let (preposition, rules) = fromEmpty (L.null test_goals) 
        debugBuild = to_env nonOptimized

    putStrLn $ "test mode with" ++ preposition ++ " args"
    runRules Loud rules debugBuild
   where
    nonOptimized :: Bool 
    nonOptimized = True

    -- | I think this is actually a use-case for arrows.
    -- It could also be reduced further with lenses.
    fromEmpty :: Bool -> (String, BuildM Rules ())
    fromEmpty True  = ("out", test_rules >> Rule.all_test_states) 
    fromEmpty False = ([], test_rules) 

fromModes :: Modes -> IO ()
fromModes modes = do 
  args <- E.getArgs
  if L.null args 
    then 
      uncurry fromMode' $ def modes 
    else
      case M.lookup (L.head args) (toMode modes) of
        Nothing            -> uncurry fromMode' (def modes)
        Just (title', io') -> E.withArgs (L.drop 1 args) $ fromMode' title' io' 
  where
    fromMode' :: String -> IO () -> IO ()
    fromMode' title' m = putStrLn ("Mode: " ++ title' ++ " Service \n") >> m
