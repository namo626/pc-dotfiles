import XMonad
import XMonad.Actions.DynamicWorkspaces (removeWorkspace)
import XMonad.Actions.CycleWS
import XMonad.Actions.CycleRecentWS
import XMonad.Util.NamedScratchpad
import XMonad.Prompt
import XMonad.Layout.ResizableTile
import XMonad.Layout.Spacing
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicLog
import XMonad.Util.Run
import XMonad.Layout.Tabbed
import XMonad.Layout  
import XMonad.Layout.SubLayouts
import XMonad.Layout.WindowNavigation
import XMonad.Layout.BoringWindows
import XMonad.Layout.Simplest
import XMonad.Layout.Circle
import XMonad.Layout.NoFrillsDecoration
import XMonad.Hooks.ManageHelpers
import XMonad.ManageHook
import XMonad.Actions.DynamicProjects
import qualified Data.Map as M
import Data.Tree
import XMonad.Actions.TreeSelect
import XMonad.Hooks.WorkspaceHistory
import qualified XMonad.StackSet as W
import XMonad.Layout.Groups.Helpers
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Grid
--import XMonad.Hooks.EwmhDesktops (fullscreenEventHook)
import XMonad.Layout.Fullscreen
import XMonad.Hooks.InsertPosition
import XMonad.Layout.Groups.Examples
import XMonad.Layout.ThreeColumns
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances

main = do 
    xmonad =<< statusBar "xmobar" myPP toggleStrutsKey (dynamicProjects projects $ fullscreenSupport $ myConfig)


myConfig = def {
      manageHook = insertPosition Below Newer <+> fullscreenManageHook <+> myManageHook <+> manageDocks <+> manageHook def
    , handleEventHook = handleEventHook def <+> docksEventHook <+> fullscreenEventHook
    , modMask = mod4Mask
    , borderWidth = 0
    , terminal = myTerminal
    , layoutHook = myLayoutHook
    , keys = myKeys <+> keys def
    , XMonad.workspaces = toWorkspaces myWorkspaces
    --, focusFollowsMouse = False
    } 

myTerminal = "terminator"
altMask = mod1Mask

myLayoutHook = onWorkspace "misc" miscLayout
             $ onWorkspace "docs" docsLayout
             $ onWorkspace "hw" hwLayout
             $ onWorkspace "free" ((ThreeColMid 1 (3/100) (1/2)) ||| ResizableTall 1 (3/100) (56/100) [])
             
             $ mainLayout

tiledTabs = tallTabs def {hNMaster = 2}

mainLayout = mkToggle (single FULL)
                      $ windowNavigation
                      $ addTabs shrinkText myTabTheme 
                      $ subLayout [] (Simplest) 
                      $ spacingWithEdge 13
                      $ ResizableTall 1 (3/100) (56/100) [] ||| Full ||| GridRatio (3/3)
--
miscLayout = windowNavigation
           $ addTabs shrinkText myTabTheme 
           $ subLayout [] (Simplest) 
           $ spacingWithEdge 9 
           $ Circle ||| Full

docsLayout = windowNavigation
                      $ addTabs shrinkText myTabTheme 
                      $ subLayout [] (Simplest) 
                      $ spacingWithEdge 15
                      $ Full ||| ResizableTall 1 (3/100) (35/100) [] 

hwLayout = windowNavigation
                      $ addTabs shrinkText myTabTheme 
                      $ subLayout [] (Simplest) 
                      $ spacingWithEdge 9 
                      $ ResizableTall 1 (3/100) (50/100) [] ||| Full 

--
--scratchpads = [ NS "thunar" "thunar" (title =? "thunar") defaultFloating]
--
myPP = xmobarPP {ppOrder = \(ws:l:t:_) -> [ws, t]}
toggleStrutsKey XConfig {XMonad.modMask = modMask} = (modMask, xK_b)

myWorkspaces :: Forest String
myWorkspaces = 
    [ Node "web" []
    , Node "term" []
    , Node "prgm" []
    , Node "hw" []
    , Node "docs" []
    , Node "matlab" []
    , Node "misc" []
    , Node "game" []
    , Node "free" []
    ]

projects :: [Project]
projects = 
    [ Project { projectName = "misc"
              , projectDirectory = "~/"
              , projectStartHook = Just $ do runInTerm "" "htop"
              }

    , Project { projectName = "term"
              , projectDirectory = "~/"
              , projectStartHook = Just $ do spawn "terminator"
                                             spawn "terminator"
                                             spawn "terminator"
              }

    , Project { projectName = "web"
              , projectDirectory = "~/"
              , projectStartHook = Just $ do spawn "firefox"
                                             spawn "terminator"
                                             spawn "terminator"
              }

    , Project { projectName = "prgm"
              , projectDirectory = "~/MEGA"
              , projectStartHook = Just $ do spawn "terminator"
                                             spawn "terminator"
                                             spawn "terminator"

              }

    , Project { projectName = "docs"
              , projectDirectory = "~/MEGA"
              , projectStartHook = Just $ do runInTerm "" "ranger"

              }

    , Project { projectName = "hw"
              , projectDirectory = "~/MEGA"
              , projectStartHook = Just $ do spawn "firefox"
                                             runInTerm "" "ranger"

              }
    
    , Project { projectName = "free"
              , projectDirectory = "~/"
              , projectStartHook = Just $ do spawn "chromium"

              }

    ]
                                          
myTabTheme = def { fontName = "xft:xos4 Terminus:style=bold:size=14"
                 , decoHeight = 35
                 , activeTextColor = "#fbf1c7"
                 , inactiveTextColor = "#ebdbb2"
                 , inactiveColor = "#504945"
                 , inactiveBorderColor = "#504945"
                 , activeColor = "#665c54"
                 , activeBorderColor = "#665c54"
                 }


myManageHook = composeAll
                [ name =? "Terminator Preferences" --> doCenterFloat
                , className =? "Thunar" --> doCenterFloat]
              
                where name = stringProperty "WM_NAME"
         
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList
            [ ((modm .|. controlMask, xK_h), sendMessage $ pullGroup L)
            , ((modm .|. controlMask, xK_l), sendMessage $ pullGroup R)
            , ((modm .|. controlMask, xK_k), sendMessage $ pullGroup U)
            , ((modm .|. controlMask, xK_j), sendMessage $ pullGroup D)
            , ((modm, xK_d), spawn "rofi -show run")
            , ((modm .|. altMask, xK_l), spawn "i3lock -c 000000") 
            , ((modm .|. controlMask, xK_m), withFocused (sendMessage . MergeAll))
            , ((modm .|. controlMask, xK_u), withFocused (sendMessage . UnMerge))
            , ((modm .|. controlMask, xK_i), withFocused (sendMessage . UnMergeAll))
            , ((altMask, xK_j), sendMessage $ Go D)
            , ((altMask, xK_k), sendMessage $ Go U)
            , ((altMask, xK_h), sendMessage $ Go L)
            , ((altMask, xK_l), sendMessage $ Go R)
            , ((modm, xK_s), switchProjectPrompt myPrompt)
            , ((modm, xK_slash), shiftToProjectPrompt myPrompt)
            , ((modm, xK_z), sendMessage MirrorExpand)
            , ((modm, xK_a), sendMessage MirrorShrink)
            , ((modm, xK_g), sequence_ $ [sendMessage $ IncMasterN 1, sendMessage $ pullGroup D, sendMessage $ IncMasterN (-1)])
            , ((altMask, xK_f), treeselectWorkspace tsDefaultConfig myWorkspaces W.greedyView)
            --, ((modm, xK_n), namedScratchpadAction scratchpads "thunar")
            , ((modm .|. shiftMask, xK_BackSpace), removeWorkspace)
            , ((modm, xK_n), moveToNewGroupUp)
            , ((modm, xK_p), splitGroup)
            , ((modm, xK_Caps_Lock), sequence_ $ [sendMessage ToggleStruts, sendMessage $ Toggle FULL])
            , ((modm .|. shiftMask, xK_Caps_Lock), sequence_ $ [withFocused (sendMessage . UnMerge), sendMessage $ pullGroup L]) 
            , ((controlMask .|. shiftMask, xK_Caps_Lock), sequence_ $ [withFocused (sendMessage . UnMerge), sendMessage $ pullGroup D]) 
            , ((modm .|. controlMask, xK_Left), sendMessage $ Swap L)

            --easy switching of workspaces
            , ((altMask, xK_Left), prevWS)
            , ((altMask, xK_Right), nextWS)
            , ((altMask, xK_Tab), cycleRecentWS [xK_Alt_L] xK_Tab xK_grave)
            ]

myPrompt = def
  { font = "xft:Droid Sans Mono for Powerline:size=13"
  , position = CenteredAt (1/2) (1/2)
  , height = 40
  }

base03  = "#002b36"
base02  = "#073642"
base01  = "#586e75"
base00  = "#657b83"
base0   = "#839496"
base1   = "#93a1a1"
base2   = "#eee8d5"
base3   = "#fdf6e3"
yellow  = "#b58900"
orange  = "#cb4b16"
red     = "#dc322f"
magenta = "#d33682"
violet  = "#6c71c4"
blue    = "#268bd2"
cyan    = "#2aa198"
green = "#859900"

--topBarTheme = def
--    { inactiveBorderColor   = base03
--    , inactiveColor         = base03
--    , inactiveTextColor     = base03
--    , activeBorderColor     = blue
--    , activeColor           = blue
--    , activeTextColor       = blue
--    , urgentBorderColor     = red
--    , urgentTextColor       = yellow
--    , decoHeight            = 10
--    }
--
