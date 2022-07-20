module MyScratchpads
(myScratchpads)
where

import XMonad
import XMonad.Hooks.DynamicProperty
import XMonad.Util.NamedScratchpad
import qualified XMonad.StackSet as W

termRunIf :: String -> String -> String -> String
termRunIf termCmd a b = "command -v " ++ a ++ ">/dev/null && " ++ run a ++ " || " ++ run b
  where run x = termCmd ++ (if null x then "" else " -e ") ++ x

myScratchpads :: String -> [NamedScratchpad]
myScratchpads terminal = [ NS "terminal" spawnTerm queryTerm manageTerm ]
  where
    spawnTerm   = termRunIf (terminal ++ " -t scratchpad") "tmux" ""
    queryTerm   = title =? "scratchpad"
    manageTerm  = customFloating $ W.RationalRect l t w h
      where
        h = 0.9
        w = 0.9
        t = 0.95 - h
        l = 0.95 - w
