{-# LANGUAGE RecordWildCards #-}

module MyKeys
(myKeys,myCheatsheetKey)
where

import XMonad
import XMonad.Layout.MultiToggle           ( Toggle(Toggle) )
import XMonad.Layout.MultiToggle.Instances ( StdTransformers(NBFULL) )
import XMonad.Layout.ResizableTile         ( MirrorResize(MirrorShrink, MirrorExpand) )
import XMonad.Util.EZConfig                ( mkNamedKeymap )
import XMonad.Util.NamedActions            ( (^++^), addName, subtitle, NamedAction )
import XMonad.Util.NamedScratchpad         ( namedScratchpadAction )
import qualified XMonad.StackSet as W

import System.Exit (exitSuccess)

import MyScratchpads
import MyCheatsheet
import MyTypes

-- Convert multiword strings to arguments (concatenate with delimiters)
-- This makes sure my shell scripts correctly interpret their arguments
args :: String -> [String] -> String
args command arguments = command ++ " " ++ unwords (map show arguments)

-- M = M1 is Super, which I have also set to space when held down
-- M3 is Hyper, which I have set to Caps Lock
-- C-Esc is Super tapped on its own

type Key = (KeyMask, KeySym)
type Keybindings = [(Key, NamedAction)]

-- Keybinding to display the keybinding cheatsheet
--myCheatsheetKey :: String -> Key
myCheatsheetKey :: KeyMask -> Key
myCheatsheetKey m = (m .|. shiftMask, xK_slash)
-- Directly using ? doesn't seem to work
--myCheatsheetKey m = (m, xK_question)

myKeys :: AppConfig -> XConfig Layout -> Keybindings
myKeys AppConfig{..} conf@XConfig{..} = let

  subKeys name list = subtitle name : mkNamedKeymap conf list

  -- Abbreviations for certain actions
  menuEditScript         = spawn $ args "menu-edit-script" [menu,editor]
  menuEditConfig         = spawn $ args "menu-edit-config" [menu,editor]
  menuChangeColourscheme = spawn $ args "menu-change-colourscheme" [menu]
  menuReadPdf            = spawn $ args "menu-read-pdf" [menu,pdfReader]

  viewScreen s          = screenWorkspace s >>= flip whenJust (windows . W.view)
  shiftScreen s         = screenWorkspace s >>= flip whenJust (windows . W.shift)
  unFloat               = withFocused $ windows . W.sink

  volumeAdjust "toggle" = spawn "adjust-volume toggle"
  volumeAdjust value    = spawn $ args "adjust-volume" $ words value

  nsTerminal  = namedScratchpadAction (myScratchpads terminal) "terminal"
  --nsMusic     = namedScratchpadAction myScratchpads "music"

  in

  subKeys "Core"
  [ ("M-S-q",                   addName "Quit XMonad (logout)"  $ io exitSuccess)
  , ("M-q",                     addName "Recompile and restart" $ spawn buildScript)
  , ("M-S-l",                   addName "Refresh layoutHook"    $ setLayout layoutHook)
  , ("M-S-s",                   addName "Suspend"               $ spawn "systemctl suspend")
  , ("C-<Escape>",              addName "Application launcher"  $ spawn "appmenu")
  , ("M-S-c",                   addName "Close window"          $ kill)
  ] ^++^

  subKeys "Screens" (
  [("M-"++key,                  addName ("Focus screen "++show sc)   $ viewScreen sc)
      | (key,sc) <- zip ["w","e","r"] [0..]
  ] ^++^
  [("M-S-"++key,                addName ("Send to screen "++show sc) $ shiftScreen sc)
      | (key,sc) <- zip ["w","e","r"] [0..]
  ]) ^++^

  subKeys "Workspaces" (
  [ ("M-"++show key,            addName ("View workspace "++i)    $ windows $ W.greedyView i)
      | (key,i) <- zip [1..9] (XMonad.workspaces conf)
  ] ^++^
  [ ("M-S-"++show key,          addName ("Send to workspace "++i) $ windows $ W.shift i)
      | (key,i) <- zip [1..9] (XMonad.workspaces conf)
  ] ^++^
  [ ("M--",                     addName "Terminal scratchpad"    $ nsTerminal)
  --, ("M-=",                     addName "Music scratchpad"       $ nsMusic)
  ]) ^++^

  subKeys "Layouts"
  [ ("M-h",                     addName "Shrink master"          $ sendMessage Shrink)
  , ("M-l",                     addName "Expand master"          $ sendMessage Expand)
  , ("M-i",                     addName "Shrink slave"           $ sendMessage MirrorExpand)
  , ("M-u",                     addName "Expand slave"           $ sendMessage MirrorShrink)
  , ("M-,",                     addName "Inc master windows"     $ sendMessage $ IncMasterN 1)
  , ("M-.",                     addName "Dec master windows"     $ sendMessage $ IncMasterN (-1))
  , ("M3-<Space>",              addName "Next layout"            $ sendMessage NextLayout)
  , ("M-f",                     addName "Toggle fullscreen"      $ sendMessage $ Toggle NBFULL)
  ] ^++^

  subKeys "Windows"
  [ ("M-<Tab>",                 addName "Focus next"             $ windows W.focusDown)
  , ("M-S-<Tab>",               addName "Focus previous"         $ windows W.focusUp)
  , ("M-j",                     addName "Focus next"             $ windows W.focusDown)
  , ("M-k",                     addName "Focus previous"         $ windows W.focusUp)
  , ("M-m",                     addName "Focus master"           $ windows W.focusMaster)
  , ("M-S-j",                   addName "Swap next"              $ windows W.swapDown)
  , ("M-S-k",                   addName "Swap previous"          $ windows W.swapUp)
  , ("M-<Return>",              addName "Swap master"            $ windows W.swapMaster)
  , ("M-t",                     addName "Unfloat"                $ unFloat)
  ] ^++^

  subKeys "Applications"
  [ ("M-S-<Return>",            addName "Terminal emulator"      $ spawn terminal)
  , ("M3-<Return>",             addName "Terminal emulator"      $ spawn terminal)
  , ("M3-v",                    addName "Vim"                    $ spawn $ terminal ++ " -e nvim")
  , ("M3-e",                    addName "Emacs"                  $ spawn "emacs")
  , ("M3-w",                    addName "Web browser (minimal)"  $ spawn browserMinimal)
  , ("M3-S-w",                  addName "Web browser (big)"      $ spawn browserBig)
  , ("M3-f",                    addName "Terminal file manager"  $ spawn fileManager)
  , ("M3-S-f",                  addName "Graphical file manager" $ spawn fileManagerGUI)
  , ("M3-z",                    addName "Zoom"                   $ spawn "zoom")
  ] ^++^

  subKeys "My Scripts"
  [ ("M-p M-p",                 addName "Edit scripts"           $ menuEditScript)
  , ("M-p M-e",                 addName "Edit configs"           $ menuEditConfig)
  , ("M-p M-c",                 addName "Change colourscheme"    $ menuChangeColourscheme)
  , ("M-p M-z",                 addName "Read PDF file"          $ menuReadPdf)
  ] ^++^

  subKeys "Multimedia Keys"
  [ ("<XF86AudioMute>",         addName "Toggle mute"            $ volumeAdjust "toggle")
  , ("<XF86AudioRaiseVolume>",  addName "Increase volume"        $ volumeAdjust "+ 5%")
  , ("<XF86AudioLowerVolume>",  addName "Decrease volume"        $ volumeAdjust "- 5%")
  , ("<XF86MonBrightnessUp>",   addName "Increase brightness"    $ spawn "adjust-brightness + 10%")
  , ("<XF86MonBrightnessDown>", addName "Decrease brightness"    $ spawn "adjust-brightness - 10%")
  , ("<Print>",                 addName "Take screenshot"        $ spawn printScreen)
  ]
