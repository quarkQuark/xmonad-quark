{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}

module MyLayoutHook
( LayoutConfig(..)
, myLayoutHook )
where

import XMonad.Hooks.ManageDocks            ( avoidStruts )
import XMonad.Layout                       ( (|||) )
import XMonad.Layout.Master                ( mastered )
import XMonad.Layout.MultiToggle           ( mkToggle, single )
import XMonad.Layout.MultiToggle.Instances ( StdTransformers(NBFULL) )
import XMonad.Layout.NoBorders             ( smartBorders )
import XMonad.Layout.Renamed               ( Rename(Replace), renamed )
import XMonad.Layout.ResizableTile         ( ResizableTall(ResizableTall) )
import XMonad.Layout.Spacing               ( spacingRaw, Border(Border) )
import XMonad.Layout.Tabbed
    ( def,
      shrinkText,
      tabbed,
      Theme(..) )
import XMonad.Layout.ThreeColumns          ( ThreeCol(ThreeColMid) )

data LayoutConfig = LayoutConfig
  { myFontName               :: String
  , mySpacingAround          :: Integer
  , mySpacingBetween         :: Integer
  , myTabActiveColor         :: String
  , myTabInactiveColor       :: String
  , myTabActiveBorderColor   :: String
  , myTabInactiveBorderColor :: String
  , myTabActiveTextColor     :: String
  , myTabInactiveTextColor   :: String
  }

myLayoutHook LayoutConfig{..} = smartBorders
             $ mkToggle (single NBFULL)
             $ tall ||| three ||| tabs ||| masterAndTabs
  where
    -- Dimensions are given as (Border top bottom right left)
    mySpacing = let a = mySpacingAround; b = mySpacingBetween in spacingRaw
        True             -- Only for >1 window
        (Border 0 a a a) -- Size of screen edge gaps
        True             -- Enable screen edge gaps
        (Border b b b b) -- Size of window gaps
        True             -- Enable window gaps

    myTabbed = tabbed shrinkText myTabTheme

    tall  = renamed [Replace "Tall"]
      $ mySpacing
      $ avoidStruts
      $ ResizableTall 1 (3/100) (1/2) []

    three = renamed [Replace "Three"]
      $ mySpacing
      $ avoidStruts
      $ ThreeColMid 1 (3/100) (1/2)

    tabs = renamed [Replace "Tabs"]
      $ avoidStruts
      $ myTabbed

    masterAndTabs = renamed [Replace "Master and Tabs"]
      $ mySpacing
      $ avoidStruts
      $ mastered (1/100) (1/2)
      $ myTabbed

    myTabTheme = def
      { activeColor         = myTabActiveColor
      , inactiveColor       = myTabInactiveColor
      , activeBorderColor   = myTabActiveBorderColor
      , inactiveBorderColor = myTabInactiveBorderColor
      , activeTextColor     = myTabActiveTextColor
      , inactiveTextColor   = myTabInactiveTextColor
      , fontName            = myFontName
      }
