import XMonad
import qualified XMonad.StackSet as W
import XMonad.Config (def)

import XMonad.Util.Run(spawnPipe, runProcessWithInput)
import XMonad.Util.SpawnOnce(spawnOnce,spawnOnOnce)
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Util.WorkspaceCompare (getSortByXineramaRule)

-- Hooks
import XMonad.Hooks.DynamicLog (dynamicLogWithPP, xmobarPP, 
                                xmobarColor,
                                ppOutput, ppTitle,
                                shorten)
import XMonad.Hooks.ManageDocks (avoidStruts, manageDocks, docks)
import XMonad.Hooks.EwmhDesktops (fullscreenEventHook)
import XMonad.Hooks.ManageHelpers (isFullscreen, doFullFloat, composeOne, (-?>))
import XMonad.Hooks.FadeInactive (fadeInactiveCurrentWSLogHook)

-- Actions
import XMonad.Actions.CycleWS 
import XMonad.Actions.CycleWorkspaceByScreen (cycleWorkspaceOnCurrentScreen)
import XMonad.Actions.DynamicWorkspaces (renameWorkspace)
import XMonad.Actions.SpawnOn (spawnOn)

-- Layouts
import XMonad.Layout (Tall)
import XMonad.Layout.NoBorders (noBorders, smartBorders)
import XMonad.Layout.Fullscreen (fullscreenFull)
import XMonad.Layout.IndependentScreens (countScreens)
import XMonad.Layout.Spacing

import System.IO
import System.Exit (exitWith, ExitCode(ExitSuccess))

import Control.Monad (when)

-- Donde van todos los archivos necesarios para la configuracion
mySystemFolder = "/run/media/rodrigo/storage/sistema"

myModMask = mod4Mask        -- Mod key es super/meta/win key
myTerminal = "alacritty"      -- Terminal por defecto
myDefaultEditor = "code"    -- Default text editor

myBorderWidth = 0       -- Ancho del borde de las ventanas
mySpacing = 5           -- Espacio entre ventanas. Es margen para cada una así que en efecto es el doble
myInactiveFade = 0.7   -- Cantidad opacidad de ventanas inactivas
myFont = "Droid Sans Mono"

-- Estos comandos son ejecutados al inicio o con refreshes
myComposer = "killall -w compton; compton --backend glx --blur-method kawase  --blur-background --blur-strength 10" -- Se encarga de todos los efectos especiales
myTray = "killall -w stalonetray; stalonetray -c $HOME/.xmonad/stalonetrayrc"
myWallpaperMgr = "feh --randomize --bg-fill " ++ mySystemFolder ++ "/wallpapers/*"


-- Nombres para los workspaces
myWorkspaces = ["web", "code", "3", "4", "5", "6", "7", "8", "9"]

myKeys = [
    -- Custom keybinds here
    -- Navegación entre workspaces
      ((myModMask,                  xK_Right),  nextWsInScreen)         -- Next WS no visible
    , ((myModMask,                  xK_Left),   prevWsInScreen)         -- Prev WS no visible
    --  ((myModMask,                  xK_Right),  cycleWorkspaceOnCurrentScreen [xK_Super_L] xK_Right xK_Left)
    --, ((myModMask,                  xK_Left),   cycleWorkspaceOnCurrentScreen [xK_Super_L] xK_Left  xK_Right)
    , ((myModMask .|. shiftMask,    xK_Right),  moveTo Next EmptyWS)    -- Proximo WS libre
    -- Navegación entre pantallas
    , ((myModMask,                  xK_Up),     nextScreen)     -- Cambia el foco a la otra pantalla
    , ((myModMask,                  xK_Down),   prevScreen)     -- Cambia el foco a la otra pantalla
    -- Cambiar workspaces entre pantallas
    , ((myModMask .|. shiftMask,    xK_Up),     swapNextScreen >> nextScreen)
    , ((myModMask .|. shiftMask,    xK_Down),   shiftWSNextScreen)
    -- Funciones Fs
    , ((myModMask,                  xK_F7),     spawn "$HOME/.scripts/displayselector.sh && xmonad --restart")
    -- Disparar programas
    , ((myModMask,                  xK_p),      myAppLauncher)
    -- Cosas de XMonad cuando se rompe
    , ((myModMask,                  xK_q),      spawn recompileXmonad)
    -- , ((myModMask .|. shiftMask,    xK_q),      promptExit)
    -- Pruebas que no funcionan :(
    , ((myModMask .|. shiftMask,    xK_r),      renameWorkspace def)
    ] ++
    viewKeys

-- Todos los programas para desplegar al inicio
myStartupHook = do
    spawnOnce "/usr/lib/pam_kwallet_init"
    spawnOnce "dunst -conf $HOME/.xmonad/dunstrc"
    spawn myComposer
    spawn myTray
    spawnOnce "nm-applet --sm-disable"  -- Network MAnager Applet en la tray
    spawnOnOnce "1" "chromium"          -- Ejecuta chromium en el ws 1
    -- spawnOnOnce "2" myTerminal       -- Ejecuta la terminal en el ws 2. NO FUNCIONA :(
    spawn myWallpaperMgr                -- Set de random Wallpapers from the forlder
    

myLayouts = addSpacing $ avoidStruts $ noBorders $
    tall
    ||| Mirror tall
    ||| noBorders (fullscreenFull Full)
    where tall  = Tall 1 (3/100) (2/3)
          addSpacing = spacingRaw True (Border 0 mySpacing mySpacing mySpacing) True (Border mySpacing mySpacing mySpacing mySpacing) True


myManageHook = composeOne [
      isFullscreen -?> doFullFloat
    , className =? "stalonetray" -?> doIgnore
    ]


-- dmenu + .desktop launchers
myAppLauncher = spawn $ "j4-dmenu-desktop --dmenu=\"dmenu -i -fn '" ++ myFont ++ "-12' -l 10\""

-- Pregunta antes de salir
-- promptExit = do
--    response <- runProcessWithInput "dmenu" ["-i","-fn","'"++myFont++"'","-p", "Salir?"] "no\nsi\n"
--    when (response == "si") (io (exitWith ExitSuccess))

main = do
    
    screensN <- countScreens    -- Cuenta el numero de pantallas
    xmbars <- startBars screensN   -- Crea mobar según el número de pantallas
    
    -- Realiza la configuración de xmonad
    xmonad $ docks def
        { modMask           = myModMask     -- Setea la tecla mod
        , terminal          = myTerminal    -- Setea la terminal por defecto
        , workspaces        = myWorkspaces  -- Setea la lista de nombres de workspaces
        -- Hooks layouts
        , layoutHook        = myLayouts -- avoidStruts $ layoutHook def
        , manageHook        = manageHook def <+> manageDocks <+> myManageHook
        , handleEventHook   = fullscreenEventHook
        , logHook           = (myLogHook xmbars)
        , startupHook       = myStartupHook
        -- Additional actions
        } `additionalKeys` myKeys -- Setea algunos shortcuts para acciones especificas
        

-- SH commands para ejecutar con spawn
notify msg = "notify-send -a 'xmonad' 'XMonad' '" ++ msg ++ "'"  -- Envia una notificación

recompileXmonad = "xmonad --recompile || " ++ notify "Error de compilación" ++ " && xmonad --restart" -- Compila reinicia XMonad


-- Reemplaza el modo greedyView por view.
-- Esto es para que no haga swap de los monitores.
viewKeys = [ 
    ((m .|. myModMask, k), windows $ f i) -- Replace 'mod1Mask' with your mod key of choice.
    | (i, k) <- zip myWorkspaces [xK_1 .. xK_9]
    , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]
    ]

-- Rota entre workspaces en la misma pantalla
wsInScreen dir = do 
    t <- findWorkspace getSortByXineramaRule dir NonEmptyWS 2
    windows . W.view $ t

moveTo' :: Direction1D -> WSType -> X ()
moveTo' dir t = doTo dir t getSortByXineramaRule (windows . W.greedyView)

nextWsInScreen = moveTo Next HiddenNonEmptyWS

prevWsInScreen = moveTo Prev HiddenNonEmptyWS

-- Envía el WS a la otra pantalla, dejando un WS vacio en la pantalla actual
shiftWSNextScreen = do
    ws <- gets (W.currentTag . windowset)
    moveTo Next EmptyWS
    nextScreen
    windows . W.view $ ws




-- Conecta las barras al windows manager
myLogHook bars = do
    mapM_ (\handler -> dynamicLogWithPP xmobarPP
          { ppOutput = hPutStrLn handler
          , ppTitle = xmobarColor "#C080C0" "" . shorten 50
          }) bars
    fadeInactiveCurrentWSLogHook myInactiveFade

-- Crea una barra (xmobar) para cada pantalla, todas con la misma configuración
startBars numberOfScreens = 
    mapM (\i -> spawnPipe $ "xmobar ~/.xmonad/xmobarrc01" ++ " -x " ++ show i) [0..numberOfScreens-1]
