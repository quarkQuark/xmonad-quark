module MyBar
where

import System.IO
import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.SpawnOnce (spawnOnce)

data Bar = Taffybar | XMobar | Tint2 deriving (Eq)
data BarConfig = BarConfig
    { barSpawnPipe :: IO Handle      -- ^ Command to start bar with handle
    , barAutostart :: X ()           -- ^ Autostart programs dependent on bar
    , barLogHook   :: Handle -> X () -- ^ Data XMonad needs to send to the bar
    }

defBarConfig = BarConfig
    { barSpawnPipe = spawnPipe ""
    , barAutostart = spawnOnce ""
    , barLogHook   = def
    }


barConfig :: Bar -> BarConfig

barConfig XMobar  = defBarConfig
    { barSpawnPipe = spawnPipe "xmobar ~/.config/xmobar/xmobarrc.hs"
    , barAutostart = spawnOnce "stalonetray --config ~/.config/stalonetray/stalonetrayrc"
      -- dynamicLogWithPP allows us to format the output
      -- xmobarPP gives us some defaults
    , barLogHook   = \h -> dynamicLogWithPP xmobarPP
          -- Write to bar instead of stdout
          { ppOutput          = hPutStrLn h
          -- How to order the different sections of the log
          , ppOrder           = \(workspace:layout:title:extras)
                              -> [workspace,layout]
          -- Separator between different sections of the log
          , ppSep             = "  "
          -- Format the workspace information
          , ppCurrent         = wsSymb "[●]" -- The workspace currently active
          , ppHidden          = wsSymb "●"   -- Workspaces with open windows
          , ppHiddenNoWindows = wsSymb "○"   -- Workspaces with no windows
          }
    }
  where
    -- xmobarPP expects function of workspace name
    wsSymb s workspace = xmobarColor "white" "" s

barConfig Tint2 = defBarConfig
    { barAutostart = spawnOnce "tint2 -c ~/.config/tint2/xmonad.tint2rc" }

barConfig Taffybar = defBarConfig
    { barAutostart = spawnOnce "taffybar" }
