module Development.Shake.Mode where
import Development.Shake
import Control.Monad.Reader.Rules
import qualified Data.Map as M
import qualified Data.List as L
import qualified System.Environment as E
import           Development.Shake.Cpp.Build
import qualified Development.Shake.Cpp.Rule as Rule

data Modes = Modes { 
  def :: (String, IO ()),
  toMode :: M.Map String (String, IO ()) 
  }

data Mode = Mode { title :: String, kw :: String, io :: IO () } 

fromModes :: Modes -> IO ()
fromModes modes = do 
  args <- E.getArgs
  if L.null args 
    then 
      uncurry fromMode $ def modes 
    else
      case M.lookup (L.head args) (toMode modes) of
        Nothing            ->
          uncurry fromMode (def modes)
        Just (title', io') ->
          E.withArgs (L.drop 1 args) $ fromMode title' io' 
  where
    fromMode :: String -> IO () -> IO ()
    fromMode title' m = putStrLn ("Build Mode: " ++ title' ++ "\n") >> m

defaultTestMode :: BuildM Rules () -> (Debug -> Env) -> Mode
defaultTestMode test_rules to_env = 
  Mode "Test Service" "test" $ do
    let debugBuild = to_env nonOptimized
    test_goals <- E.getArgs

    if L.null test_goals -- run all failing or changed tests
      then do 
        putStrLn "test mode without args"
        runRules Loud
          (test_rules >> Rule.all_test_states) 
          debugBuild 
      else do  
        putStrLn "test mode with args"
        runRules Loud test_rules debugBuild
   where
     nonOptimized = True

defaultModes :: Mode -> Modes
defaultModes test_mode = 
  Modes
    (title defaultMode, io defaultMode)
    (M.fromList $
      ("release", ("Release", putStrLn "TODO."))
      : map toEntry [test_mode])
  where
    toEntry :: Mode -> (String, (String, IO ()))
    toEntry m = (kw m, (title m, io m)) 
    defaultMode = test_mode  
