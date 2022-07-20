import XMonad                             -- standard xmonad library
import XMonad.Config.Desktop              -- default desktopConfig
import XMonad.Hooks.EwmhDesktops (ewmh)   -- Fixes the automatic fullscreening of applications
import XMonad.Util.NamedActions (addDescrKeys')
import XMonad.Util.SpawnOnce (spawnOnce)  -- For running autostart only once (on login)

import MyBar
import MyCheatsheet
import MyKeys
import MyLayoutHook
import MyManageHook
import MyTypes

main :: IO ()
main = do
    barProc <- barSpawnPipe $ barConfig myBar-- Start myBar and return a handle
    spawn "pkill -o taffybar" -- Kill oldest taffybar instance (move to M-q binding?)

    -- Applies this config file over the default config for desktop use
    xmonad
        -- Increased compliance with the Extended Window Manager Hints standard
        $ ewmh
        -- Add keybindings in such a way as to allow viewing a cheatsheet with M-?
        $ addDescrKeys' (myCheatsheetKey myModMask, myCheatsheet) (myKeys myAppConfig)
        $ myConfig barProc

  where
    myConfig barProc = desktopConfig
        { modMask            = myModMask
        , terminal           = myTerminal
        , borderWidth        = myBorderWidth
        , normalBorderColor  = myNormalBorderColour
        , focusedBorderColor = myFocusedBorderColour
        , manageHook         = myManageHook myTerminal
        , layoutHook         = myLayoutHook myLayoutConfig
        , logHook            = barLogHook (barConfig myBar) barProc
        , workspaces         = myWorkspaces
        , startupHook        = do barAutostart $ barConfig myBar
                                  spawnOnce $ myXMonadDir ++ "src/autostart.sh"
        }

    myModMask    = mod4Mask -- Super (Windows) key
    myXMonadDir  = "~/Projects/xmonad-quark/"
    myTerminal   = "alacritty"
    myBar        = XMobar
    myWorkspaces = ["1","2","3","4","5","6","7","8","9"]

    myAppConfig = AppConfig
      { browser        = "qutebrowser"
      , buildScript    = myXMonadDir ++ "build"
      , editor         = "emacs"
      , fileManager    = myTerminal ++ " -e ranger "
      , fileManagerGUI = "pcmanfm"
      , menu           = "rofi -dmenu -i -p"
      , pdfReader      = "zathura"
      , printScreen    = "spectacle"
      }

    myBorderWidth = 2
    myFocusedBorderColour = "#268bd2"
    myNormalBorderColour  = "#111111"

    -- Colours copied from DistroTube's config (at gitlab/dwt1)
    myLayoutConfig = LayoutConfig
      { myFontName               = "xft:Ubuntu Nerd Font:size=10"
      , mySpacingAround          = 20
      , mySpacingBetween         = 8
      , myTabActiveColor         = "#46D9FF"
      , myTabInactiveColor       = "#313846"
      , myTabActiveBorderColor   = "#46D9FF"
      , myTabInactiveBorderColor = "#282C34"
      , myTabActiveTextColor     = "#282C34"
      , myTabInactiveTextColor   = "#D0D0D0"
      }
