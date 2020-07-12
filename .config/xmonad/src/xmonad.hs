--------------------------------------------------------------------------------
-- IMPORTS
--------------------------------------------------------------------------------

import Data.List.Split (chunksOf)         -- Requires the package 'split'
import Prelude hiding (lookup)            -- to avoid confusing errors when mistyping Map.lookup
import System.Exit (exitSuccess)
import System.IO
import Test.FitSpec.PrettyPrint (columns) -- Requires the package 'fitspec'
import XMonad                             -- standard xmonad library
import XMonad.Config.Desktop              -- default desktopConfig
import XMonad.Hooks.DynamicLog            -- Customising the logHook (sends data to xmobar)
import XMonad.Hooks.EwmhDesktops          -- Fixes the automatic fullscreening of applications
import XMonad.Hooks.ManageDocks           -- Manipulate and avoid docks and panels
import XMonad.Layout.NoBorders            -- Remove borders when unnecessary (smartBorders)
import XMonad.Layout.Spacing              -- Gaps around windows
import qualified XMonad.StackSet as W
import XMonad.Util.EZConfig               -- Simpler keybinding syntax
import XMonad.Util.NamedActions           -- AwesomeWM-style keybinding syntax
import XMonad.Util.Run                    -- Start and send information to processes
import XMonad.Util.SpawnOnce              -- For running autostart only once (on login)

--------------------------------------------------------------------------------
-- VARIABLES AND DEFAULT PROGRAMS
--------------------------------------------------------------------------------

-- The modifier key to be used for most keybindings
-- I have it set to super (the Windows key)
myModMask :: KeyMask
myModMask  = mod4Mask

-- Default applications
myTerminal       = "alacritty"
myEditor         = myTerminal ++ " -e nvim "
myBrowser        = "qutebrowser"
myGuiFileManager = "pcmanfm"
myPdfReader      = "zathura"
myPrintScreen    = "spectacle"

-- Status bar
data Bar = Taffybar | XMobar
myBar :: Bar
myBar = Taffybar

myBarCommand :: Bar -> String
myBarCommand XMobar   = "xmobar " ++ myConfigDir ++ "xmobarrc.hs"
myBarCommand Taffybar = "taffybar"

myTrayCommand :: Bar -> String
myTrayCommand XMobar   = "stalonetray --config " ++ myConfigDir ++ "stalonetrayrc"
myTrayCommand Taffybar = "status-notifier-watcher"  -- From status-notifier-item

myLauncher, myMenu :: [Char]
-- dmenu is a much simpler option, but with less eye-candy
--myLauncher = "dmenu_run"
--myMenu     = "dmenu -i -p"
-- rofi looks much nicer, but is less minimal and the default theme is ugly
myLauncher = "rofi -show drun -theme " ++ rofiTheme "blurry-icons-centre" -- Command to open applications
myMenu     = "rofi -dmenu -i -p"  -- For scripts that require user input

-- Config locations
myConfigDir   = "~/.config/xmonad/src/"       -- XMonad-related config
myBuildScript = "~/.config/xmonad/build"      -- Script to recompile and restart xmonad
myAutostart   = myConfigDir ++ "autostart.sh" -- Script to run on login
rofiTheme theme = "~/.config/rofi/themes/" ++ theme ++ ".rasi" -- Rofi theme directory

--------------------------------------------------------------------------------
-- FUNCTIONS AND SCRIPTS
--------------------------------------------------------------------------------

-- Convert multiword strings to arguments (concatenate with delimiters)
-- This makes sure my shell scripts correctly interpret their arguments
args :: String -> [String] -> String
args command arguments = command ++ " " ++ unwords (map show arguments)

--------------------------------------------------------------------------------
-- KEYBINDINGS
--------------------------------------------------------------------------------

myKeys :: XConfig l -> [((KeyMask, KeySym), NamedAction)]
myKeys conf = let

    subKeys name list = subtitle name : mkNamedKeymap conf list

    -- Abbreviations for certain actions
    menuEditScript         = spawn $ args "menu-edit-script" [myMenu,myEditor]
    menuEditConfig         = spawn $ args "menu-edit-config" [myMenu,myEditor]
    menuChangeColourscheme = spawn $ args "menu-edit-colourscheme" [myMenu]
    menuReadPdf            = spawn $ args "menu-read-pdf" [myMenu,myPdfReader]

    volumeAdjust "toggle" = spawn "adjust-volume toggle"
    volumeAdjust value    = spawn $ args "adjust-volume" $ words value

    brightnessAdjust percentage = spawn
        $ "xbacklight " ++ percentage ++ " && notify-send \"Brightness `xbacklight -get`%\""

    in

    subKeys "Core"
    [ ("M-S-q",      addName "Quit XMonad (logout)" $ io exitSuccess)
    , ("M-q",        addName "Recompile & restart"  $ spawn myBuildScript)
    -- I use xcape to remap C-Esc to the super key on its own
    , ("C-<Escape>", addName "Application launcher" $ spawn myLauncher)
    , ("<Print>",    addName "Take screenshot"      $ spawn myPrintScreen)
    , ("M-S-c",      addName "Close window"         $ kill)
    ] ^++^

    -- Untested as I currently only have one screen
    -- Also, the default keys clash with some of mine
    --subKeys "Screens" (
    --[("M-"++key,   addName "Focus screen"
                  -- $ screenWorkspace sc >>= flip whenJust (windows . W.view))
       -- | (key,sc) <- zip ["w","e","r"] [0..]
    --] ^++^
    --[("M-S-"++key, addName "Send to screen"
                  -- $ screenWorkspace sc >>= flip whenJust (windows . W.shift))
       -- | (key,sc) <- zip ["w","e","r"] [0..]
    --]) ^++^

    subKeys "Workspaces" (
    [ ("M-"++show key, addName ("View workspace " ++ i) $ windows $ W.greedyView i)
        | (key,i) <- zip [1..9] (XMonad.workspaces conf)
    ] ^++^
    [ ("M-S-"++show key, addName ("Send to workspace " ++ i) $ windows $ W.shift i)
        | (key,i) <- zip [1..9] (XMonad.workspaces conf)
    ]) ^++^

    subKeys "Layouts"
    [ ("M-S-f",       addName "Toggle fullscreen"    $ sendMessage ToggleStruts)
    , ("M-h",         addName "Shrink master"        $ sendMessage Shrink)
    , ("M-l",         addName "Expand master"        $ sendMessage Expand)
    , ("M-,",         addName "More master windows"  $ sendMessage $ IncMasterN 1)
    , ("M-.",         addName "Fewer master windows" $ sendMessage $ IncMasterN (-1))
    , ("M-<Space>",   addName "Next layout"          $ sendMessage NextLayout)
    ] ^++^

    subKeys "Windows"
    [ ("M-<Tab>",    addName "Focus next"     $ windows W.focusDown)
    , ("M-S-<Tab>",  addName "Focus previous" $ windows W.focusUp)
    , ("M-j",        addName "Focus next"     $ windows W.focusDown)
    , ("M-k",        addName "Focus previous" $ windows W.focusUp)
    , ("M-m",        addName "Focus master"   $ windows W.focusMaster)
    , ("M-S-j",      addName "Swap next"      $ windows W.swapDown)
    , ("M-S-k",      addName "Swap previous"  $ windows W.swapUp)
    , ("M-<Return>", addName "Swap master"    $ windows W.swapMaster)
    , ("M-t",        addName "Unfloat"        $ withFocused $ windows . W.sink)
    ] ^++^

    subKeys "Applications"
    [ ("M-S-<Return>", addName "Terminal emulator"      $ spawn myTerminal)
    , ("M-e",          addName "Text editor"            $ spawn myEditor)
    , ("M-w",          addName "Web browser (minimal)"  $ spawn myBrowser)
    , ("M-S-w",        addName "Firefox"                $ spawn "firefox")
    , ("M-f",          addName "Graphical file manager" $ spawn myGuiFileManager)
    , ("M-z",          addName "Zoom"                   $ spawn "zoom")
    ] ^++^

    subKeys "My Scripts"
    [ ("M-p M-p", addName "Edit scripts"        $ menuEditScript)
    , ("M-p M-e", addName "Edit configs"        $ menuEditConfig)
    , ("M-p M-c", addName "Change colourscheme" $ menuChangeColourscheme)
    , ("M-p M-z", addName "Read PDF file"       $ menuReadPdf)
    ] ^++^

    subKeys "Multimedia Keys"
    [ ("<XF86AudioMute>",         addName "Toggle mute"         $ volumeAdjust "toggle")
    , ("<XF86AudioLowerVolume>",  addName "Decrease volume"     $ volumeAdjust "- 5%")
    , ("<XF86AudioRaiseVolume>",  addName "Increase volume"     $ volumeAdjust "+ 5%")
    , ("<XF86MonBrightnessDown>", addName "Decrease brightness" $ brightnessAdjust "-dec 10")
    , ("<XF86MonBrightnessUp>",   addName "Increase brightness" $ brightnessAdjust "-inc 10")
    , ("C-<F1>",                  addName "Toggle mute"         $ volumeAdjust "toggle")
    , ("C-<F2>",                  addName "Decrease volume"     $ volumeAdjust "- 5%")
    , ("C-<F3>",                  addName "Increase volume"     $ volumeAdjust "+ 5%")
    , ("C-<F11>",                 addName "Decrease brightness" $ brightnessAdjust "-dec 10")
    , ("C-<F12>",                 addName "Increase brightness" $ brightnessAdjust "-inc 10")
    ]

-- Keybinding to display the keybinding cheatsheet
myCheatsheetKey :: (KeyMask, KeySym)
myCheatsheetKey = (myModMask .|. shiftMask, xK_slash)

-- Number of colomns with with which to display the cheatsheet
myCheatsheetCols :: Int
myCheatsheetCols = 3

-- Format the keybindings so they can be sent to the display
formatList :: [String] -> String
formatList list = columns "SeparatorPlaceholder" -- Normalise column widths -> Table
                $ map unlines -- Connect the sublists with line breals -> [column1,column2,...]
                $ chunksOf (myCheatsheetRows (list))
                $ list -- The list to be formatted

        where rowsFromColumns list nCol = 1 + length list `div` nCol
              myCheatsheetRows list = rowsFromColumns list myCheatsheetCols

-- How to display the cheatsheet (from Ethan Schoonover's config)
showKeybindings :: [((KeyMask, KeySym), NamedAction)] -> NamedAction
showKeybindings myKeyList = addName "Show Keybindings" $ io $ do
    handle <- spawnPipe "dzen2-display-cheatsheet"
    hPutStrLn handle "TitlePlaceholder\n" -- Replaced in the script
    hPutStrLn handle $ formatList (showKm myKeyList)
    hClose handle
    return ()

--------------------------------------------------------------------------------
-- AESTHETICS
--------------------------------------------------------------------------------

-- Gaps around and between windows
-- Changes only seem to apply if I log out then in again
-- Dimensions are given as (Border top bottom right left)
mySpacing = spacingRaw True             -- Only for >1 window
                       -- The bottom edge seems to look narrower than it is
                       (Border 0 15 10 10) -- Size of screen edge gaps
                       True             -- Enable screen edge gaps
                       (Border 5 5 5 5) -- Size of window gaps
                       True             -- Enable window gaps

myBorderWidth :: Dimension
myBorderWidth = 2

myNormalBorderColour, myFocusedBorderColour :: [Char]
myNormalBorderColour = "#111111"
myFocusedBorderColour = "#268bd2"

--------------------------------------------------------------------------------
-- LOGHOOK
-- The information to send to xmobar, through the handle we defined earlier
--------------------------------------------------------------------------------

-- Symbols for displaying workspaces in xmobar
-- Must be functions, as it expects a different symbol for each
myCurrentWsSymbol workspaceName = "[●]" -- The workspace currently active
myHiddenWsSymbol  workspaceName =  "●"  -- Workspaces with open windows
myEmptyWsSymbol   workspaceName =  "○"  -- Workspaces with no windows

-- bar points to the status bar's process handle
-- XMonad.Hooks.DynamicLog (dynamicLogWithPP) allows us to format the output
-- XMonad.Hooks.DynamicLog (xmobarPP) gives us some defaults
myLogHook bar = dynamicLogWithPP xmobarPP
        -- Write to bar instead of stdout
        { ppOutput          = hPutStrLn bar
        -- How to order the different sections of the log
        , ppOrder           = \(workspace:layout:title:extras)
                            -> [workspace,layout]
        -- Separator between different sections of the log
        , ppSep             = "  "
        -- Format the workspace information
        , ppCurrent         = xmobarColor "white" "" . myCurrentWsSymbol
        , ppHidden          = xmobarColor "white" "" . myHiddenWsSymbol
        , ppHiddenNoWindows = xmobarColor "white" "" . myEmptyWsSymbol
        }

--------------------------------------------------------------------------------
-- MANAGEHOOK
-- special rules based on window types
--------------------------------------------------------------------------------

myManageHook :: ManageHook
myManageHook = composeAll . concat $
    -- Windows to automatically float
    [ [ className =? c --> doFloat                 | c <- myFloatClasses ]
    , [ title     =? t --> doFloat                 | t <- myFloatTitles ]
    ]
  -- To find a window class or title, run xprop in a terminal, then click on it
  where myFloatClasses = ["Gimp","conky","plasmashell","vlc","Caprine", "Nitrogen"]
        myFloatTitles  = ["Whisker Menu"]

--------------------------------------------------------------------------------
-- WORKSPACES
--------------------------------------------------------------------------------

-- My workspaces are currently just numbers
myWorkspaces :: [String]
myWorkspaces = ["1","2","3","4","5","6","7","8","9"]

--------------------------------------------------------------------------------
-- MAIN
-- putting it all together
--------------------------------------------------------------------------------

-- This is the part that is actually run as a window manager
main :: IO ()
main = do
    -- spawnPipe starts the bar and returns a handle - named barProc - for input
    barProc <- spawnPipe (myBarCommand myBar)

    -- Applies this config file over the default config for desktop use
    xmonad
        -- Increased compliance with the Extended Window Manager Hints standard
        $ ewmh
        -- Use my config, with the process handle for xmobar
        -- Add keybindings in such a way as to allow viewing a cheatsheet with M-?
        -- showKeybindings is the script used to display them
        -- The prime shows that we are not merging with the default keys
        $ addDescrKeys' (myCheatsheetKey, showKeybindings) myKeys
        $ myConfig barProc

-- Adding all of my stuff to the default desktop config
myConfig bar = desktopConfig
        -- Variables
        { modMask  = myModMask
        , terminal = myTerminal
        -- Borders
        , borderWidth        = myBorderWidth
        , normalBorderColor  = myNormalBorderColour
        , focusedBorderColor = myFocusedBorderColour
        -- Hooks
        , manageHook  = manageDocks <+> manageHook desktopConfig <+> myManageHook
        , layoutHook  = avoidStruts $ mySpacing $ smartBorders (layoutHook desktopConfig)
        , logHook     = myLogHook bar
        , workspaces  = myWorkspaces
        , startupHook = do
            spawnOnce (myTrayCommand myBar)
            spawnOnce myAutostart
        }
