module MyManageHook
(myManageHook)
where

import Data.List (isInfixOf)
import XMonad
import XMonad.Hooks.ManageDocks

myManageHook :: ManageHook
myManageHook = manageSpecific <+> manageDocks

titleContains :: String -> Query Bool
titleContains string = fmap (isInfixOf string) title

-- To find a window class or title, run xprop in a terminal, then click on it
manageSpecific :: ManageHook
manageSpecific = composeAll . concat $
    [ [ className  =? c                           --> doFloat | c <- myFloatClasses ]
    , [ title      =? t                           --> doFloat | t <- myFloatTitles ]
    , [ className  =? "zoom" <&&> titleContains z --> doFloat | z <- myZoomFloats ]
    , [ isZoomNotification                        --> doFloat ]
    ]
    where
        myFloatClasses = [ "Gimp"
                         , "conky"
                         , "plasmashell"
                         , "vlc"
                         , "Nitrogen"
                         , "Tint2conf"
                         , "dolphin-emu"
                         , "zulip"
                         , "discord"
                         , "element"
                         , "Steam"
                         , "heroic"
                         ]
        myFloatTitles  = ["Whisker Menu"]
        myZoomFloats   = ["Chat", "Participants", "Rooms"]
        isZoomNotification = className =? "zoom" <&&> title =? "zoom"
