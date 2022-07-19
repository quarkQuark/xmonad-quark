module Options where

myXMonadDir :: String
myXMonadDir = "~/Projects/xmonad-quark/"

data Bar = Taffybar | XMobar | Tint2 deriving (Eq)
myBar :: Bar
myBar = XMobar

--------------------------------------------------------------------------------
-- FILEPATHS

myConfigDir       = myXMonadDir ++ "src/"       -- XMonad-related config
myAutostart       = myXMonadDir ++ "src/autostart.sh" -- Script to run on login
myXMobarConf      = "~/.config/xmobar/xmobarrc.hs"
myStalonetrayConf = "~/.config/stalonetray/stalonetrayrc"
myTint2Conf       = "~/.config/tint2/xmonad.tint2rc"
