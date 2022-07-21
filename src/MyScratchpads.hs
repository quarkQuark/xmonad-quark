module MyScratchpads
(nsTerminal, manageScratchpads)
where

import XMonad
import XMonad.Hooks.DynamicProperty
import XMonad.Util.NamedScratchpad
import qualified XMonad.StackSet as W

manageScratchpads :: String -> ManageHook
manageScratchpads term = namedScratchpadManageHook $ myScratchpadsCLI term ++ myScratchpadsGUI

nsTerminal :: String -> X ()
nsTerminal term = namedScratchpadAction (myScratchpadsCLI term) "terminal"

--nsMusic     = namedScratchpadAction myScratchpads "music"

myScratchpadsCLI :: String -> NamedScratchpads
myScratchpadsCLI term = [ NS "terminal" spawnTerm queryTerm manageTerm ]
  where
    -- Run a if it exists or b otherwise
    termRunIf :: String -> String -> String -> String
    termRunIf termCmd a b = let run x = if null x then termCmd ++ x else termCmd ++ " -e " ++ x in
      "command -v " ++ a ++ ">/dev/null && " ++ run a ++ " || " ++ run b

    spawnTerm   = termRunIf (term ++ " --title scratchpad") "tmux" ""
    queryTerm   = title =? "scratchpad"
    manageTerm  = customFloating $ W.RationalRect l t w h
      where
        h = 0.9
        w = 0.9
        t = 0.95 - h
        l = 0.95 - w

myScratchpadsGUI :: NamedScratchpads
myScratchpadsGUI = []
