module MyTypes
  ( AppConfig(..)
  , CommandWithPrefix
  , Key
  , Keybindings
  )
where

import XMonad
import XMonad.Util.NamedActions

type CommandWithPrefix = Either (String,String) String

data AppConfig = AppConfig
  { browserMinimal :: !String
  , browserBig     :: !String
  , buildScript    :: !String -- ^ Script to recompile and restart xmonad
  , editor         :: !String
  , fileManager    :: !CommandWithPrefix
  , menu           :: !String -- ^ For scripts that require user input
  , pdfReader      :: !String
  , printScreen    :: !String
  }

type Key = (KeyMask, KeySym)
type Keybindings = [(Key, NamedAction)]
