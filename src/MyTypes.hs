module MyTypes
( AppConfig(..) )
where

data AppConfig = AppConfig
  { browserMinimal :: !String
  , browserBig     :: !String
  , buildScript    :: !String -- ^ Script to recompile and restart xmonad
  , editor         :: !String
  , fileManager    :: !String
  , fileManagerGUI :: !String
  , menu           :: !String -- ^ For scripts that require user input
  , pdfReader      :: !String
  , printScreen    :: !String
  }
