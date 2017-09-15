import XMonad
import XMonad.Layout.Hidden
import XMonad.Actions.DynamicWorkspaces (removeWorkspace)
import XMonad.Actions.CycleWS
import XMonad.Actions.CycleRecentWS
import XMonad.Actions.Navigation2D
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
import XMonad.Layout.Fullscreen
import XMonad.Hooks.InsertPosition
import XMonad.Layout.Groups.Examples
import XMonad.Layout.ThreeColumns
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances

main = do 
    xmonad =<< statusBar "xmobar" myPP toggleStrutsKey (withNavigation2DConfig def $ dynamicProjects projects $ fullscreenSupport $ myConfig)


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
    --, startupHook = spawn "stalonetray"
    } 

myTerminal = "terminator"
altMask = mod1Mask
addTopBar = noFrillsDeco shrinkText topBarTheme

myLayoutHook = avoidStruts $ mkToggle (single FULL)
             $ windowNavigation
             $ onWorkspace "misc" miscLayout 
             $ onWorkspace "docs" docsLayout 
             $ onWorkspace "hw" hwLayout
             $ onWorkspace "free" Full
             $ mainLayout 

tiledTabs = tallTabs def {hNMaster = 2}
mainLayout = 
    mkToggle (single FULL)
    $ windowNavigation
    $ addTopBar
    $ addTabs shrinkText myTabTheme 
    $ subLayout [] (Simplest) 
    $ spacingWithEdge 13
    $ hiddenWindows
    $ ResizableTall 1 (3/100) (56/100) [] ||| Full

octaveLayout = 
    mkToggle (single FULL)
    $ windowNavigation   
    $ addTopBar
    $ addTabs shrinkText myTabTheme 
    $ subLayout [] (Simplest) 
    $ spacingWithEdge 13
    $ hiddenWindows
    $ ResizableTall 1 (3/100) (50/100) [] ||| Full ||| GridRatio (3/3) ||| Grid

miscLayout = 
    mkToggle (single FULL)
    $ windowNavigation
    $ addTopBar
    $ addTabs shrinkText myTabTheme 
    $ subLayout [] (Simplest) 
    $ spacingWithEdge 9 
    $ Circle ||| Full

docsLayout = 
    mkToggle (single FULL) 
    $ windowNavigation
    $ addTopBar
    $ addTabs shrinkText myTabTheme 
    $ subLayout [] (Simplest) 
    $ spacingWithEdge 15
    $ ResizableTall 1 (3/100) (40/100) [] ||| Full

hwLayout = 
    mkToggle (single FULL)
    $ windowNavigation
    $ addTopBar
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
    [ Node "conf" []
    , Node "term" []
    , Node "prgm" []
    , Node "hw" []
    , Node "docs" []
    , Node "matlab" []
    , Node "misc" []
    , Node "game" []
    , Node "web" []
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

    , Project { projectName = "conf"
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
    
    , Project { projectName = "web"
              , projectDirectory = "~/"
              , projectStartHook = Just $ do spawn "firefox"

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

topBarTheme = def
    { inactiveBorderColor   = "#3c3836"
    , inactiveColor         = "#3c3836"
    , inactiveTextColor     = "#3c3836"
    , activeBorderColor     = "#458588"
    , activeColor           = "#458588"
    , activeTextColor       = "#458588"
    , urgentBorderColor     = red
    , urgentTextColor       = yellow
    , decoHeight            = 15
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
            , ((modm, xK_d), spawn "rofi -show run -font \"Droid Sans Mono for Powerline 20\"")
            , ((modm .|. altMask, xK_l), spawn "i3lock -c 000000") 
            , ((modm .|. controlMask, xK_m), withFocused (sendMessage . MergeAll))
            , ((modm .|. controlMask, xK_u), withFocused (sendMessage . UnMerge))
            , ((modm .|. controlMask, xK_i), withFocused (sendMessage . UnMergeAll))
            --, ((altMask, xK_j), sendMessage $ Go D)
            --, ((altMask, xK_k), sendMessage $ Go U)
            --, ((altMask, xK_h), sendMessage $ Go L)
            --, ((altMask, xK_l), sendMessage $ Go R)
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
            , ((modm, xK_grave), sequence_ $ [sendMessage ToggleStruts, sendMessage $ Toggle FULL])
            , ((modm .|. shiftMask, xK_Tab), sequence_ $ [withFocused (sendMessage . UnMerge), sendMessage $ pullGroup L]) 
            , ((controlMask .|. shiftMask, xK_Tab), sequence_ $ [withFocused (sendMessage . UnMerge), sendMessage $ pullGroup D]) 

            --easy swapping of windows
            , ((modm .|. shiftMask, xK_h), windowSwap L True)
            , ((modm .|. shiftMask, xK_l), windowSwap R True)
            , ((modm .|. shiftMask, xK_k), windowSwap U True)
            , ((modm .|. shiftMask, xK_j), windowSwap D True)
            , ((altMask, xK_j), windowGo D True)
            , ((altMask, xK_k), windowGo U True)
            , ((altMask, xK_h), windowGo L True)
            , ((altMask, xK_l), windowGo R True)

            --easy switching of workspaces
            , ((altMask, xK_Left), prevWS)
            , ((altMask, xK_Right), nextWS)
            , ((altMask, xK_Tab), cycleRecentWS [xK_Alt_L] xK_Tab xK_grave)

            --hiding windows
            , ((modm, xK_backslash), withFocused hideWindow)
            , ((modm .|. shiftMask, xK_backslash), popNewestHiddenWindow)
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


