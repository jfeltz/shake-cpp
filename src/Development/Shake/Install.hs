module Development.Shake.Install where
--import Control.Applicative ((<$>))
import Development.Shake
import Development.Shake.Iso
import Development.Shake.FilePath
-- import Development.Shake.Rule
-- import qualified Filesystem.Path.CurrentOS as FS
import qualified Data.List as L
import           Control.Monad.Reader

toInclude :: String -> String
toInclude local_pfx = local_pfx </> "include"

data Install = Install {
  outputPfx  :: FilePath, -- ^ /home/jpf/local/project-deps
  includes   :: Iso  -- ^ stores test pass states
}
        
installPaths :: String -> Install
installPaths project_name =
  Install {
    outputPfx  = "/home/jpf/local/project-deps",
    includes   = Iso "src" project_name 
  } 

type InstallM m a = ReaderT Install m a 

install :: (Monad m) => InstallM m Install 
install = ask 
      
incOutputDir :: (Monad m, Functor m) => InstallM m FilePath 
incOutputDir = do 
  paths <- install
  return $ outputPath (includes paths) (outputPfx paths)
  
-- | Convert output include target to source include
-- path
fromTarget :: Install -> FilePath -> FilePath
fromTarget paths tgt_include =  
  (input . includes $ paths) </> 
    (joinPath . drop dropped $ splitPath tgt_include)
  where
    dropped :: Int
    dropped = 
      L.length . splitPath $ 
        outputPfx paths </> (output . includes) paths 

-- Install rules that are ran when bound 
-- TODO handle binary case etc, i suspect that will just go in 
-- a different monad though.

shakeInstall :: InstallM Rules ()
shakeInstall = do 
  paths <- install
  lift . action $ do 
    headers <-
     getDirectoryFiles "" [(input . includes $ paths) ++ "//*"++ ".hh"]
    -- "Need" the output 
    let morph = morphRight ".hh" (outputPfx paths) (includes paths)
    need [ morph h | h <- headers ]

  -- Rule for handling header file installs
  lift $ 
   (outputPath (includes paths) (outputPfx paths) ++ "//*.hh") 
      *> \tgt_header -> do 
       -- drop the directories until the src directory is reached 
       let src_header = fromTarget paths tgt_header
       need [src_header]
       cmd "install" "-m 644" src_header tgt_header  
