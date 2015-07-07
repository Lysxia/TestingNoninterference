import System.Process
import Distribution.Simple
import Distribution.PackageDescription
main = defaultMainWithHooks simpleUserHooks
  { preBuild = \_args _buildFlags -> do
      callCommand "cd luck ; sh prepro.sh"
      return emptyHookedBuildInfo }
    
