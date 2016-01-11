import XMonad
import XMonad.Util.EZConfig
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.UrgencyHook
import XMonad.Layout.WorkspaceDir
import XMonad.Layout.Accordion
import XMonad.Layout.Grid
import XMonad.Layout.ResizableTile
import XMonad.Layout.Combo
import XMonad.Layout.LayoutBuilder
import XMonad.Prompt (defaultXPConfig, XPConfig(..))
import XMonad.Prompt.Shell
import XMonad.Util.Run(spawnPipe)

import System.IO
import System.Posix.User
import Data.List (isPrefixOf)
import System.Posix.Env (setEnv)
import qualified XMonad.StackSet as W

myConfig xmobarPipe layoutHook =
  withUrgencyHook NoUrgencyHook defaultConfig
    {
      terminal          = "~/.xmonad/gnome-terminal-wrapper --hide-menubar",
      focusFollowsMouse = False,
      workspaces        = myWorkspaces,
      layoutHook        = layoutHook,
      manageHook        = composeAll [ manageDocks ] <+> manageDocks,
      logHook           = dynamicLogWithPP $ xmobarPP {
                              ppOutput = hPutStrLn xmobarPipe,
                              ppTitle  = xmobarColor "green" "" . shorten 50,
                              ppOrder  = \(ws:l:t:_) -> [ws,l,t],
                              ppUrgent = xmobarColor "yellow" "red" . xmobarStrip
                            },
     modMask           = myModMask 
    } `removeKeysP` [("M-p")] `additionalKeysP` myKeys 

-- mod4Mask is Windows key, mod1Mask is alt
myModMask = mod1Mask

myWorkspaces = ["Mail", "gord", "workspace", "work2", "prod", "dev", "dev2", "grass", "vnc", "0", "w", "e"]

layoutPostProcess layout = avoidStruts $ workspaceDir "~" $ layout

myLayoutHook = 
  layoutPostProcess $
--    tallSplit |||
    tiled |||
    Mirror tiled |||
    Full |||
    Accordion |||
    Mirror( (layoutN   2 (relBox  0  0 (3/12) 1) (Just $ relBox 0 0 (1/2) 1) $ (ResizableTall 1 (2/100) (1/3) []) )--Tall 0 0.01 0.5)
          $ (layoutN   3 (relBox (1/4) 0 (8/12) 1) (Just $ relBox 0 0 (1/3) (2/3)) $ Mirror clocktiled )--Tall 0 0.01 0.5)
          $ (layoutAll   (relBox (3/4) 0 1 1) $ tiled )-- 0 0.01 0.5)
    )

  where
    tiled = ResizableTall 1 (2/100) (1/2) []
    tiled2 = ResizableTall 2 (2/100) (1/3) []
    clocktiled = ResizableTall 1 (2/100) (1/3) []
--    tallSplit = Tall 2 (2/100) (1/2)

runXmonad xmobarPipe = xmonad $ myConfig xmobarPipe myLayoutHook

uidbox = "tot-qws-u12058a"

rackmountKey =
  [("M-S-m", spawn command)]
    where command =
            "~/.xmonad/gnome-terminal-wrapper --hide-menubar -e 'ssh -K -Y "
            ++ uidbox ++ "'"

totowaKey =
  [("M-S-d", spawn command)]
    where command =
            "~/.xmonad/gnome-terminal-wrapper --hide-menubar -e 'ssh -K -Y "
            ++ "tot-qpr-app8" ++ "'"
    
monitoringKey =
  [("M-S-a", spawn command)]
    where command =
            "~/.xmonad/gnome-terminal-wrapper --hide-menubar -e 'ssh tot-qpr-app8' & "
            ++ "sleep 0.5; ~/.xmonad/gnome-terminal-wrapper --hide-menubar & "
            ++ "sleep 0.5; ~/.xmonad/gnome-terminal-wrapper --hide-menubar -e 'catalog br tot:/app/cateye/views/gord-primary' & "
            ++ "sleep 0.5; ~/.xmonad/gnome-terminal-wrapper --hide-menubar --profile=bill-sm -e 'eye market-clock -s' & "
            ++ "sleep 0.5; ~/.xmonad/gnome-terminal-wrapper --hide-menubar -e 'eye support-oak' & "
            ++ "sleep 0.5; ~/.xmonad/gnome-terminal-wrapper --hide-menubar -e 'eye broadcast -m' & "
           

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
  rackmountKey 
  ++
  totowaKey
  ++
  monitoringKey
  ++
  [ ("M-" ++ ws, windows $ W.greedyView ws) | ws <- myWorkspaces ]
  ++
  [ ("M-S-" ++ ws, windows $ W.shift ws) | ws <- myWorkspaces ]
  ++
  [ ("M-" ++ [key], screenWorkspace scr >>= flip whenJust (windows . W.view))
    | (key, scr)  <- zip "hjkl" [0,1,2,3] -- change to match your screen order
  ]


setEnvironment = 
  -- Google's Chrome uses xdg-open to open saved files.
  -- xdg-open doesn't know about xmonad, so it feeds things to
  -- firefox.
  -- Hard-coding things to gnome
  setEnv "DE" "gnome" False

main =
  do
    setEnvironment
    spawn "xscreensaver -no-splash"
    xmobarPipe <- spawnPipe "xmobar ~/.xmonad/xmobarrc"
    spawn "xmobar ~/.xmonad/xmobarrc2"
    runXmonad xmobarPipe 
