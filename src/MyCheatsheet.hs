{-# LANGUAGE OverloadedStrings #-}

module MyCheatsheet
where

import Data.List
import Data.List.Split (chunksOf)
import Text.Printf
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Text (Text)
import System.IO
--import Test.FitSpec.PrettyPrint (columns) -- Requires the 'fitspec' package

import XMonad
import XMonad.Util.NamedActions
import XMonad.Util.Run

import MyTypes

import Control.Monad (liftM3)

-- Number of colomns with with which to display the cheatsheet
myCheatsheetCols :: Int
myCheatsheetCols = 3

myCheatsheetRows :: [a] -> Int
myCheatsheetRows xs = 1 + length xs `div` myCheatsheetCols

manageCheatsheet :: ManageHook
manageCheatsheet = title =? "cheatsheet" --> doFloat

-- Probably worth putting in a named scratchpad so I can toggle visibility. Then i can do-block writeFile + action to toggle nsp, so hat it rewrites every time. Can be hidden from bar.
myCheatsheet :: Keybindings -> NamedAction
myCheatsheet myKeyList = addName "Show keybindings" $ do
    io $ T.writeFile ".cache/xmonad/keybindings" $ prettyColumns myKeyList
    spawn "kitty --title cheatsheet -e less ~/.cache/xmonad/keybindings"

prettyColumns :: Keybindings -> Text
prettyColumns = T.unlines
              . joinColumns
              . map (map colourMods)
              . splitColumns
              . T.lines . T.unlines . map T.pack . showKm
  where
    joinColumns = leftPad . removeTrailingSpaces . map (T.intercalate colPad)
     where
       leftPad = map ("  " <>)
       colPad  = ""
       removeTrailingSpaces = map (T.reverse . T.dropWhile (==' ') . T.reverse)

    splitColumns xs = ( transpose
                      . map normaliseCols . normaliseRows
                      . chunksOf (myCheatsheetRows xs)
                      ) xs

    normaliseRows xs = map (pad (maxLength xs)) xs
      where
        pad l x   = x ++ replicate (l - length x) ""
        maxLength = maximum . (0:) . map length

    normaliseCols xs = map (pad (maxLength xs)) xs
      where
        pad l x   = x <> T.replicate (l - T.length x) " "
        maxLength = maximum . (0:) . map T.length

    -- removeExtraSpacing :: [Text] -> [Text]
    -- removeExtraSpacing xs = map (T.take (maxLength xs - (maxLength . map T.stripEnd) xs)) xs
    --   where
    --     maxLength = maximum . (0:) . map T.length

data Colour
  = Black       | Red           | Green       | Yellow
  | BrightBlack | BrightRed     | BrightGreen | BrightYellow
  | Blue        | Magenta       | Cyan        | White
  | BrightBlue  | BrightMagenta | BrightCyan  | BrightWhite

-- | Get the ansi keycode for a colour
ansiColour :: Colour -> Int
ansiColour c = case c of
  Black       -> 30; Red           -> 31; Green       -> 32; Yellow       -> 33
  BrightBlack -> 90; BrightRed     -> 91; BrightGreen -> 92; BrightYellow -> 93
  Blue        -> 34; Magenta       -> 35; Cyan        -> 36; White        -> 37
  BrightBlue  -> 94; BrightMagenta -> 95; BrightCyan  -> 96; BrightWhite  -> 97

-- | Colour a string using an ansi escape sequence
colourText :: Colour -> Text -> Text
colourText c x = T.concat ["\ESC[", (T.pack . show . ansiColour) c, ";1m", x, "\ESC[m"]

-- | Colour only the first word
colourWord :: Colour -> Text -> Text
colourWord c x = case T.breakOn " " x of
  ("",_) -> colourText c x
  (_,"") -> colourText c x
  (a,b)  -> colourText c a <> b

-- | Colour a modified key, removing the modifier
colourMod :: Text -> Colour -> Text -> Text
colourMod m c x = if m `T.isPrefixOf` x
  then T.concat (loop (-1) c (T.splitOn m x))
  -- then T.concat $ loop c (T.splitOn m x) ++ replicate (T.count m x * T.length m) " "
  else x
  where
    loop n c (x:xs) = colourWord c x : loop (n+1) c xs
    loop n c xs     = T.replicate (n * T.length m) " " : xs

-- | Colour lines beginning with the specified pattern
doFrom :: Text -> (Text -> Text) -> Text -> Text
doFrom p f x = if p `T.isPrefixOf` x then f x else x

-- | Apply appropriate colours to a line
colourMods :: Text -> Text
colourMods = colourMod "M3-" Yellow
           . colourMod "M4-" Magenta
           . doFrom ">" (colourText BrightWhite)
           . doFrom "<" (colourWord Cyan)
           . T.replace "S-/" "?  "
           . T.replace "C-<Esc>" (colourText Yellow "Hyper  ")
