{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant $" #-}

module MyKeys
  ( myKeys
  , myCheatsheetKey
  , terminalApp
  )
where

import XMonad
import XMonad.Actions.Prefix
import XMonad.Layout.MultiToggle           ( Toggle(Toggle) )
import XMonad.Layout.MultiToggle.Instances ( StdTransformers(NBFULL) )
import XMonad.Layout.ResizableTile         ( MirrorResize(MirrorShrink, MirrorExpand) )
import XMonad.Util.EZConfig                ( mkNamedKeymap )
import XMonad.Util.NamedActions            ( (^++^), addName, subtitle, NamedAction )
import XMonad.Util.NamedScratchpad         ( namedScratchpadAction )
import qualified XMonad.StackSet as W

import System.Exit (exitSuccess)
import Data.List   (stripPrefix)
import Text.Printf (printf)

import MyScratchpads
import MyCheatsheet
import MyTypes ( AppConfig(..), CommandWithPrefix )

-- | Convert multiword strings to arguments (concatenate with delimiters)
-- This makes sure my shell scripts correctly interpret their arguments
args :: String -> [String] -> String
args command arguments = command ++ " " ++ unwords (map show arguments)

-- M = M1 is Super, which I have also set to space when held down
-- M3 is Hyper, which I have set to Caps Lock
-- C-Esc is Super tapped on its own

type Key = (KeyMask, KeySym)
type Keybindings = [(Key, NamedAction)]

-- | Keybinding to display the keybinding cheatsheet
--myCheatsheetKey :: String -> Key
myCheatsheetKey :: KeyMask -> Key
myCheatsheetKey m = (m .|. shiftMask, xK_slash)
-- Directly using ? doesn't seem to work
--myCheatsheetKey m = (m, xK_question)

-- | Command to run a terminal application
terminalApp :: String -> String -> String
terminalApp t x = t ++ " -e " ++ x

myKeys :: AppConfig -> XConfig Layout -> Keybindings
myKeys AppConfig{..} conf@XConfig{..} = let

  subKeys name list = subtitle name : mkNamedKeymap conf list

  -- Abbreviations for certain actions
  menuEditScript         = spawn $ args "menu-edit-script" [menu,editor]
  menuEditConfig         = spawn $ args "menu-edit-config" [menu,editor]
  menuChangeColourscheme = spawn $ args "menu-change-colours" [menu]
  menuReadPdf            = spawn $ args "menu-read-pdf" [menu,pdfReader]

  viewScreen s  = screenWorkspace s >>= flip whenJust (windows . W.view)
  shiftScreen s = screenWorkspace s >>= flip whenJust (windows . W.shift)

  myRestart :: PrefixArgument -> X ()
  myRestart (Raw _) = spawn buildScript >> setLayout layoutHook
  myRestart _       = spawn buildScript

  namedWithPrefixSpawn :: String -> CommandWithPrefix -> NamedAction
  namedWithPrefixSpawn s a = addName (s ++ (either f1 f2 a)) $ withPrefixSpawn a
    where
      f1 (a,b) = printf ": %s (%s)" (cmd a) (cmd b)
      f2 s     = printf ": %s" (cmd s)
      cmd s = toName s $ stripPrefix (terminalApp terminal "") s
      toName s (Just x) = x
      toName s Nothing  = s
  
  withPrefixSpawn :: CommandWithPrefix -> X ()
  withPrefixSpawn = either f1 f2
    where
      f1 (a,b) = withPrefixArgument $ \u -> spawn $ if (isPrefixRaw u) then b else a
      f2 s = withPrefixArgument $ \u -> spawn s

  spawnTUI :: String -> X ()
  spawnTUI = spawn . terminalApp terminal

  in

  subKeys "Core"
  [ ("M-S-q",           addName "Quit XMonad (logout)"        $ io exitSuccess)
  , ("M-q",             addName "Recompile and restart"       $ withPrefixArgument myRestart)
  , ("M-S-s",           addName "Suspend"                     $ spawn "systemctl suspend")
  , ("C-<Escape>",      addName "Application launcher"        $ spawn "appmenu")
  , ("M-S-c",           addName "Close window"                $ kill)
  ] ^++^

  subKeys "Screens"
  (concat
   [ [("M-"++k,         addName ("Focus screen "++show sc)    $ viewScreen sc)
     , ("M-S-"++k,      addName ("Send to screen "++show sc)  $ shiftScreen sc)
   ] | (k,sc) <- zip ["w","e","r"] [0..] ]
  ) ^++^

  subKeys "Workspaces"
  (concat
   [ [("M-"++show k,    addName ("View workspace "++i)        $ windows $ W.greedyView i)
     , ("M-S-"++show k, addName ("Send to workspace "++i)     $ windows $ W.shift i)
   ] | (k,i) <- zip [1..9] workspaces ] ^++^
   [ ("M--",            addName "Terminal scratchpad"         $ nsTerminal terminal)
   --, ("M-=",            addName "Music scratchpad"           $ nsMusic)
   ]
  ) ^++^

  subKeys "Layouts"
  [ ("M-h",             addName "Shrink master"               $ sendMessage Shrink)
  , ("M-l",             addName "Expand master"               $ sendMessage Expand)
  , ("M-i",             addName "Shrink slave"                $ sendMessage MirrorExpand)
  , ("M-o",             addName "Expand slave"                $ sendMessage MirrorShrink)
  , ("M-,",             addName "Inc master windows"          $ sendMessage $ IncMasterN 1)
  , ("M-.",             addName "Dec master windows"          $ sendMessage $ IncMasterN (-1))
  , ("M3-<Space>",      addName "Next layout"                 $ sendMessage NextLayout)
  , ("M-f",             addName "Toggle fullscreen"           $ sendMessage $ Toggle NBFULL)
  ] ^++^

  subKeys "Windows"
  [ ("M-j",             addName "Focus next"                  $ windows W.focusDown)
  , ("M-k",             addName "Focus previous"              $ windows W.focusUp)
  , ("M-m",             addName "Focus master"                $ windows W.focusMaster)
  , ("M-S-j",           addName "Swap next"                   $ windows W.swapDown)
  , ("M-S-k",           addName "Swap previous"               $ windows W.swapUp)
  , ("M-<Return>",      addName "Swap master"                 $ windows W.swapMaster)
  , ("M-t",             addName "Unfloat"                     $ withFocused $ windows . W.sink)
  ] ^++^

  subKeys "Applications"
  [ ("M-S-<Return>",    addName "Terminal emulator"           $ spawn terminal)
  , ("M3-<Return>",     addName "Terminal emulator"           $ spawn terminal)
  , ("M3-v",            addName "Vim"                         $ spawnTUI "nvim")
  , ("M3-e",            addName "Emacs"                       $ spawn "emacs")
  , ("M3-w",            addName "Web browser (minimal)"       $ spawn browserMinimal)
  , ("M3-S-w",          addName "Web browser (big)"           $ spawn browserBig)
  , ("M3-f",            namedWithPrefixSpawn "File manager"   $ fileManager)
  , ("M3-z",            addName "Zoom"                        $ spawn "zoom")
  ] ^++^

  subKeys "My Scripts"
  [ ("M-p M-p",         addName "Edit scripts"                $ menuEditScript)
  , ("M-p M-e",         addName "Edit configs"                $ menuEditConfig)
  , ("M-p M-c",         addName "Change colourscheme"         $ menuChangeColourscheme)
  , ("M-p M-z",         addName "Read PDF file"               $ menuReadPdf)
  ] ^++^

  subKeys "Multimedia Keys"
  [ ("<XF86AudioMute>",         addName "Toggle mute"         $ spawn "adjust volume toggle")
  , ("<XF86AudioRaiseVolume>",  addName "Increase volume"     $ spawn "adjust volume +10%")
  , ("<XF86AudioLowerVolume>",  addName "Decrease volume"     $ spawn "adjust volume -10%")
  , ("<XF86MonBrightnessUp>",   addName "Increase brightness" $ spawn "adjust brightness +10%")
  , ("<XF86MonBrightnessDown>", addName "Decrease brightness" $ spawn "adjust brightness -10%")
  , ("<Print>",                 addName "Take screenshot"     $ spawn printScreen)
  ]
