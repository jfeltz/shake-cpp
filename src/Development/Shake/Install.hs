-- TODO Install data-structure is C++ specific, so this should be moved 

module Development.Shake.Install where
import Development.Shake
import Development.Shake.Iso
import Development.Shake.FilePath
import           Control.Monad.Reader

toInclude :: String -> String
toInclude local_pfx = local_pfx </> "include"

-- TODO libs, shared etc
data Install = Install {
  outputPfx  :: FilePath, -- ^ /home/jpf/local/project-deps
  headerExt  :: FilePath, -- ^ e.g. hh 
  includes   :: Iso  -- ^ stores test pass states
}
        
type InstallM m a = ReaderT Install m a 

install :: (Monad m) => InstallM m Install 
install = ask 
      
incOutputDir :: (Monad m, Functor m) => InstallM m FilePath 
incOutputDir = do 
  paths <- install
  return $ outputPath (includes paths) (outputPfx paths)
  
-- Install rules that are ran when bound 
-- TODO handle binary case etc, i suspect that will just go in 
-- a different monad though.

installRules :: InstallM Rules ()
installRules = do 
  paths <- install
  lift . action $ do 
    headers <-
      getDirectoryFiles "" [(input . includes $ paths) ++ headerLeafs paths]
    -- "Need" the output corresponding to input headers 
    let 
      toOutput = 
        morphRight 
          (headerExt paths)
          (outputPfx paths) 
          (includes paths) 
          . dropDirectory1
    need [ toOutput h | h <- headers ]

  -- Rule for handling header file installs
  lift $ 
   (outputPath (includes paths) (outputPfx paths) ++ headerLeafs paths)
      *> \tgt_header -> do
       let 
        src_header =
          morphLeft 
            (headerExt paths)
            (outputPfx paths) 
            (includes paths) 
            tgt_header
       need [src_header]
       cmd "install" "-m 644" src_header tgt_header
  where
    headerLeafs :: Install -> String
    headerLeafs = (++) "//*." . headerExt
