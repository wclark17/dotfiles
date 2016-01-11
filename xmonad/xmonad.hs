-- default desktop configuration for Fedora

import System.Posix.Env (getEnv)
import Data.Maybe (maybe)

import XMonad
import XMonad.Util.EZConfig
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.UrgencyHook
import XMonad.Layout.WorkspaceDir
import XMonad.Layout.ResizableTile
import XMonad.Prompt (defaultXPConfig, XPConfig(..))
import XMonad.Prompt.Shell
import XMonad.Util.Run(spawnPipe)

import System.IO
import System.Posix.User
import Data.List (isPrefixOf)
import System.Posix.Env (setEnv)
import qualified XMonad.StackSet as W
import XMonad.Config.Desktop
import XMonad.Config.Gnome
import XMonad.Config.Kde
import XMonad.Config.Xfce

myConfig layoutHook =
  withUrgencyHook NoUrgencyHook defaultConfig
    {
      terminal          = "mate-terminal",
      focusFollowsMouse = False,
      layoutHook        = layoutHook,
      manageHook        = composeAll [ manageDocks ] <+> manageDocks,
     modMask           = myModMask 
    } `removeKeysP` [("M-p")] `additionalKeysP` myKeys 

-- mod4Mask is Windows key, mod1Mask is alt
myModMask = mod1Mask

layoutPostProcess layout = avoidStruts $ workspaceDir "~" $ layout

myWorkspaces = ["1", "2", "3", "4", "5", "6", "7", "8", "9", "0"]

myLayoutHook = 
  layoutPostProcess $
--    tallSplit |||
    tiled |||
    Mirror tiled |||
    Full 

  where
    tiled = ResizableTall 1 (2/100) (1/2) []
    tiled2 = ResizableTall 2 (2/100) (1/3) []
    clocktiled = ResizableTall 1 (2/100) (1/3) []
--    tallSplit = Tall 2 (2/100) (1/2)

runXmonad = xmonad $ myConfig myLayoutHook

myKeys =
  {- bindings below are for both qwerty and dvorak -}
  [
    ("M-=", sendMessage Expand),
    ("M--", sendMessage Shrink),
    ("M-S-=", sendMessage MirrorExpand),
    ("M-S--", sendMessage MirrorShrink),
    ("M-u", windows W.focusUp),
    ("M-i", windows W.focusDown),
    ("M-g", windows W.focusDown),
    ("M-c", windows W.focusUp),
    ("M-d", changeDir defaultXPConfig),
    ("M-S-r", shellPrompt defaultXPConfig),
    ("M-S-t", withFocused $ windows . W.sink),
    ("M-S-l", spawn "xscreensaver-command -l") ]
  ++
  [ ("M-" ++ ws, windows $ W.greedyView ws) | ws <- myWorkspaces ]
  ++
  [ ("M-S-" ++ ws, windows $ W.shift ws) | ws <- myWorkspaces ]
  ++
  [ ("M-" ++ [key], screenWorkspace scr >>= flip whenJust (windows . W.view))
    | (key, scr)  <- zip "hl" [0,1] -- change to match your screen order
  ]


--setEnvironment = 
  -- Google's Chrome uses xdg-open to open saved files.
  -- xdg-open doesn't know about xmonad, so it feeds things to
  -- firefox.
  -- Hard-coding things to gnome
--  setEnv "DE" "gnome" False

main =
  do
--    setEnvironment
    spawn "xscreensaver -no-splash"
    runXmonad 



--main = do
--     session <- getEnv "DESKTOP_SESSION"
--     xmonad  $ maybe desktopConfig desktop session

desktop "gnome" = gnomeConfig
desktop "kde" = kde4Config
desktop "xfce" = xfceConfig
desktop "xmonad-mate" = gnomeConfig
desktop _ = desktopConfig
