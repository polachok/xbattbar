module XBattBar (start) where
import Types
import Backend
import Data.Word
import Data.Bits
import Control.Monad
import Graphics.X11.Types
import Graphics.X11.Xlib.Types
import Graphics.X11.Xlib.Display
import Graphics.X11.Xlib.Window
import Graphics.X11.Xlib.Event
import Graphics.X11.Xlib.Misc
import Graphics.X11.Xlib.Context
import Graphics.X11.Xlib.Color
import System.Posix.IO.Select
import System.Posix.Types

data XBattBar = XBattBar {
                    dpy :: Display,
                    screen :: ScreenNumber,
                    window :: Window,
                    x :: Position,
                    y :: Position,
                    w :: Dimension,
                    h :: Dimension,
                    gc :: GC,
                    options :: Options
                }

getX dpy screen opts = case position opts of
                    "right" -> fromIntegral $ (displayWidth dpy screen) - (fromIntegral $ thickness opts)
                    _ -> 0

getY dpy screen opts = case position opts of
                    "bottom" -> fromIntegral $ (displayHeight dpy screen) - (fromIntegral $ thickness opts)
                    _ -> 0

getW dpy screen opts = case position opts of
                    "left" -> fromIntegral $ thickness opts
                    "right" -> fromIntegral $ thickness opts
                    _ -> fromIntegral $ displayWidth dpy screen

getH dpy screen opts = case position opts of
                    "top" -> fromIntegral $ thickness opts
                    "bottom" -> fromIntegral $ thickness opts
                    _ -> fromIntegral $ displayHeight dpy screen

getFG dpy screen opts state = do
    (color, _) <- allocNamedColor dpy (defaultColormap dpy screen)
                   ((case state of 
                        Battery -> chargeColorBat
                        AC -> chargeColorAC) opts)
                                        
    return $ color_pixel color

getBG dpy screen opts state = do
    (color, _) <- allocNamedColor dpy (defaultColormap dpy screen) 
                   ((case state of 
                        Battery -> dischargeColorBat
                        AC -> dischargeColorAC) opts)
    return $ color_pixel color

start :: Options -> IO ()
start opts = do
    dpy <- openDisplay ""
    let screen = defaultScreen dpy
    root <- rootWindow dpy screen
    let borderWidth = 0
        attrmask = cWOverrideRedirect
    window <- allocaSetWindowAttributes $
        \attrs -> do 
                    set_override_redirect attrs True
                    createWindow dpy root
                                (getX dpy screen opts)
                                (getY dpy screen opts)
                                (getW dpy screen opts)
                                (getH dpy screen opts)
                                borderWidth
                                (defaultDepth dpy screen)
                                inputOutput
                                (defaultVisual dpy screen)
                                attrmask
                                attrs
    gc <- createGC dpy window
    let xbb = XBattBar dpy screen window (getX dpy screen opts)
                                         (getY dpy screen opts)
                                         (getW dpy screen opts)
                                         (getH dpy screen opts)
                                         gc
                                         opts
    run xbb
    return ()

draw :: XBattBar -> Double -> Power -> IO ()
draw xbb charge state = do
    let dpy' = dpy xbb
        screen' = screen xbb
        window' = window xbb
        gc' = gc xbb
        x' = x xbb
        y' = y xbb
        w' = w xbb
        h' = h xbb
    fg <- getFG dpy' screen' (options xbb) state
    bg <- getBG dpy' screen' (options xbb) state
    setForeground dpy' gc' bg
    fillRectangle dpy' window' gc' x' y' w' h'

    let w'' = floor $ (fromIntegral w') * charge
    setForeground dpy' gc' fg
    fillRectangle dpy' window' gc' x' y' w'' h'
    flush dpy'

handleEvents :: XBattBar -> Double -> Power -> IO ()
handleEvents xbb charge state = do
    let dpy' = dpy xbb
    n <- pending dpy'
    case n of
        0 -> return ()
        _ -> allocaXEvent $ \e -> do
                draw xbb charge state
                nextEvent dpy' e

selectWrapper fd eventH timeoutH xbb c s = do
    n <- select [fd] [] [] (Time 5 0)
    case n of 
        -1 -> error "select() error"
        0 ->  timeoutH xbb c s
        _ ->  eventH xbb c s

run :: XBattBar -> IO ()
run xbb = do
    let dpy' = dpy xbb
        window' = window xbb
    selectInput dpy' window' (exposureMask .|. visibilityChangeMask .|. structureNotifyMask)
    storeName dpy' window' "xbattbar"
    mapWindow dpy' window'
    sync dpy' False
    let fd = Fd $ connectionNumber dpy'
    forever $ do
        c <- getCharge
        s <- getPower
        selectWrapper fd handleEvents draw xbb c s
