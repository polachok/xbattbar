module XBattBar.Core (start) where
import Prelude hiding (Left, Right)
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
import Graphics.X11.Xlib.Extras (unmapWindow)
import System.Posix.IO.Select
import System.Posix.Types

import XBattBar.Types
import XBattBar.Backend
import XBattBar.Widgets

data XBattBar = XBattBar {
                    options  :: Options,
                    bar      :: ProgressBar,
                    colorAC  :: (Pixel, Pixel),
                    colorBat :: (Pixel, Pixel)
                }

getScreenRect :: XContext -> Rectangle
getScreenRect ctx = Rectangle 0 0 sw sh
                            where dpy' = dpy ctx
                                  screen' = screen ctx
                                  sw = fromIntegral $ displayWidth dpy' screen'
                                  sh = fromIntegral $ displayHeight dpy' screen'

getWindowRect :: Position -> Dimension -> Rectangle -> Rectangle
getWindowRect pos th rect = case pos of
                        Top -> rect { rect_height = th }
                        Bottom -> getWindowRect Top th $ rect { rect_y = fromIntegral $ rect_height rect - th }
                        Left -> rect { rect_width = th }
                        Right -> getWindowRect Left th $ rect { rect_x = fromIntegral $ rect_width rect - th }

getColors :: XContext -> Options -> IO ((Pixel, Pixel), (Pixel, Pixel))
getColors ctx opts = do
    let dpy' = dpy ctx
        screen' = screen ctx
        allocColor = allocNamedColor dpy' (defaultColormap dpy' screen')
    a1 <- (allocColor $ chargeColorAC opts) >>= (\(c,_) -> return $ color_pixel c)
    a2 <- (allocColor $ dischargeColorAC opts) >>= (\(c,_) -> return $ color_pixel c) 
    b1 <- (allocColor $ chargeColorBat opts) >>= (\(c,_) -> return $ color_pixel c)
    b2 <- (allocColor $ dischargeColorBat opts) >>= (\(c,_) -> return $ color_pixel c)
    return ((a1, a2), (b1, b2))

start :: Options -> IO ()
start opts = do
    dpy <- openDisplay ""
    let screen = defaultScreen dpy
    root <- rootWindow dpy screen
    let ctx = XContext dpy screen root
    let geom = getWindowRect Top (fromIntegral $ thickness opts) (getScreenRect ctx)
    let fg = whitePixel dpy screen
    let bg = blackPixel dpy screen
    bar' <- mkProgressBar ctx geom fg bg Horizontal exposureMask
    (ac, bat) <- getColors ctx opts
    let xbb = XBattBar opts bar' ac bat
    run xbb
    return ()

handleTimeout :: XBattBar -> Double -> Power -> IO ()
handleTimeout xbb charge state = drawWidget (bar xbb)

handleEvents :: XBattBar -> Double -> Power -> IO ()
handleEvents xbb charge state = do
    let bar' = bar xbb
        dpy' = dpy $ xContext bar'
    n <- pending dpy'
    case n of 
        0 -> return ()
        _ -> allocaXEvent $ \e -> do
                nextEvent dpy' e
                t <- get_EventType e
                print t
                handleWidgetEvent bar' e t

selectWrapper fd int eventH timeoutH = do
    n <- select [fd] [] [] (Time int 0)
    case n of 
        -1 -> error "select() error"
        0 ->  return timeoutH
        _ ->  return eventH

applyState :: XBattBar -> Double -> Power -> XBattBar
applyState xbb charge state = 
    let bar' = bar xbb
        (af, ab) = colorAC xbb
        (bf, bb) = colorBat xbb
    in case state of
        AC -> xbb { bar = bar' { colorBack = ab, colorBar = af, progress = charge }}
        Battery -> xbb { bar = bar' { colorBack = bb, colorBar = bf, progress = charge }}

run :: XBattBar -> IO ()
run xbb = do
    let bar' = bar xbb
        dpy' = dpy $ xContext bar'
        int  = interval $ options xbb
    displayWidget bar'
    sync dpy' False
    let fd = Fd $ connectionNumber dpy'
    forever $ do
        c <- getCharge
        s <- getPower
        let xbb' = applyState xbb c s
        selectWrapper fd int handleEvents handleTimeout >>= (\h -> h xbb' c s)
