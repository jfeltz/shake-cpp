module Control.Monad.Reader.Rules where
import Control.Monad.Reader
import Development.Shake

-- | Utility function for runing shake rules within a
-- a ReaderT 
runRules :: Verbosity -> ReaderT r Rules () -> r -> IO ()
runRules v rules = 
  shakeArgs (shakeOptions { shakeVerbosity = v }) . runReaderT rules
