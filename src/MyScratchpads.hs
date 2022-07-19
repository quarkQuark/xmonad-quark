module MyScratchpads
(myScratchpads)
where

import XMonad
import XMonad.Hooks.DynamicProperty
import XMonad.Util.NamedScratchpad
import qualified XMonad.StackSet as W

myScratchpads :: String -> [NamedScratchpad]
myScratchpads terminal = [ NS "terminal" spawnTerm queryTerm manageTerm ]
  where
    spawnTerm   = terminal ++ " -t scratchpad -e tmux"
    queryTerm   = title =? "scratchpad"
    manageTerm  = customFloating $ W.RationalRect l t w h
      where
        h = 0.9
        w = 0.9
        t = 0.95 - h
        l = 0.95 - w
