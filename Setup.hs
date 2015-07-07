import System.Process
import Distribution.Simple
main = defaultMainWithHooks simpleUserHooks
  { buildHook = \pkgdescr localbuildinfo userhooks buildflags -> do
      callCommand "cd luck ; sh prepro.sh"
      buildHook simpleUserHooks pkgdescr localbuildinfo userhooks buildflags }
    
