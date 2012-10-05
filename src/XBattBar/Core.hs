module XBattBar.Core (start) where
import Prelude hiding (Left, Right)
import XBattBar.Types
import XBattBar.Backend
import Data.Word
import Data.Bits
import Control.Monad
import Graphics.X11.Types
import Graphics.X11.Xlib.Types hiding (Position)
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
                    geom :: Rectangle,
                    gc :: GC,
                    options :: Options
                }

getScreenRect :: Display -> ScreenNumber -> Rectangle
getScreenRect dpy screen = Rectangle 0 0 sw sh
                            where sw = fromIntegral $ displayWidth dpy screen
                                  sh = fromIntegral $ displayHeight dpy screen

getWindowRect :: Position -> Dimension -> Rectangle -> Rectangle
getWindowRect pos th rect = case pos of
                        Top -> rect { rect_height = th }
                        Bottom -> getWindowRect Top th $ rect { rect_y = fromIntegral $ rect_height rect - th }
                        Left -> rect { rect_width = th }
                        Right -> getWindowRect Left th $ rect { rect_x = fromIntegral $ rect_width rect - th }

getIndicatorRect :: Position -> Double -> Rectangle -> Rectangle
getIndicatorRect pos perc rect = case pos of
                        Top -> rect { rect_x = p (rect_width rect) - fromIntegral (rect_width rect), rect_y = 0 }
                        Bottom -> rect { rect_x = p (rect_width rect) - fromIntegral (rect_width rect), rect_y = 0 }
                        _ -> rect { rect_y = fromIntegral (rect_height rect) - p (rect_height rect), rect_x = 0 }
                        where p x = floor $ perc * fromIntegral x

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
        geom = getWindowRect (position opts) (fromIntegral $ thickness opts) (getScreenRect dpy screen)
    window <- allocaSetWindowAttributes $
        \attrs -> do 
                    set_override_redirect attrs True
                    createWindow dpy root
                                (rect_x geom)
                                (rect_y geom)
                                (rect_width geom)
                                (rect_height geom)
                                borderWidth
                                (defaultDepth dpy screen)
                                inputOutput
                                (defaultVisual dpy screen)
                                attrmask
                                attrs
    gc <- createGC dpy window
    let xbb = XBattBar dpy screen window geom gc opts
    run xbb
    return ()

draw :: XBattBar -> Double -> Power -> IO ()
draw xbb charge state = do
    let dpy' = dpy xbb
        screen' = screen xbb
        window' = window xbb
        gc' = gc xbb
        geom' = geom xbb
        pos' = position $ options xbb
    fg <- getFG dpy' screen' (options xbb) state
    bg <- getBG dpy' screen' (options xbb) state
    setForeground dpy' gc' bg
    fillRectangles dpy' window' gc' [geom']
    setForeground dpy' gc' fg
    fillRectangles dpy' window' gc' [getIndicatorRect pos' charge geom']
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
    let i = interval $ options xbb
    n <- select [fd] [] [] (Time i 0)
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
